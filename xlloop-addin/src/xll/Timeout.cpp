/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "Timeout.h"
#include <ocidl.h>
#include <olectl.h>
#include <commctrl.h>

#pragma comment(lib, "comctl32.lib")

#define XLLOOP_WND_CLASS "XLLoop.BusyClass"
#define XLLOOP_WND_NAME "XLLoop.BusyWindow"
#define CLICK_MESSAGE "click to cancel"
#define CLICK_MESSAGE_LEN 15
#define IMG_SIZE 40

namespace 
{
	bool g_Initialised = false;
	HBITMAP g_SpinBitmaps[12];
	HFONT g_Font;
	HINSTANCE g_hInstance;
	HWND g_Parent;
	volatile bool g_Started = false;
	volatile bool g_Shutdown = false;
	char g_Message[MAX_PATH];
	int g_MessageLength;
	int g_CurrentImage;
	int g_Width;
	int g_Height = 60;
	HWND g_hWnd;
	RECT g_Fill;
};

LRESULT CALLBACK MainWndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

DWORD WINAPI Timeout::ShowInternal(LPVOID)
{
	g_Started = true;

	// Calculate window width
	HDC hDC = GetDC(NULL);
	HFONT of = (HFONT) SelectObject(hDC, g_Font);
	SIZE s;
	GetTextExtentPoint(hDC, g_Message, g_MessageLength, &s);
	g_Width = IMG_SIZE + s.cx + 35;
	SelectObject(hDC, of);
	g_Fill.left = 1;
	g_Fill.right = g_Width - 1;
	g_Fill.top = 1;
	g_Fill.bottom = g_Height - 1;

	// Create the window
	RECT r;
	GetWindowRect(g_Parent, &r);
	g_hWnd = CreateWindowEx(
		WS_EX_TOOLWINDOW | WS_EX_NOPARENTNOTIFY, 
		XLLOOP_WND_CLASS, 
		XLLOOP_WND_NAME, 
		WS_POPUP | WS_VISIBLE, 
		r.left + (r.right - r.left)/2 - g_Width/2, 
		r.top + (r.bottom - r.top)/2 - 60/2,
		g_Width, g_Height, NULL, NULL, NULL, NULL);

	ShowWindow(g_hWnd, SW_SHOW);
	UpdateWindow(g_hWnd);

	// Message pump
	MSG msg;
	while(true) {
		while (PeekMessage(&msg, g_hWnd, 0, 0, PM_REMOVE)) {
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
		if(g_Shutdown) break;
		InvalidateRect(g_hWnd, NULL, FALSE);
		Sleep(100);
	}

	ShowWindow(g_hWnd, SW_HIDE);
	DestroyWindow(g_hWnd);
	while (PeekMessage(&msg, g_hWnd, 0, 0, PM_REMOVE)) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	g_Started = false;

	return 0;
}

void Timeout::Init()
{
	g_CurrentImage = 0;
	g_Shutdown = false;
}

void Timeout::Show(HWND parent, const char* message)
{
	if(!g_Initialised) return;
	if(g_Started) return;

	g_CurrentImage = 0;
	g_Shutdown = false;
	g_Parent = parent;
	strcpy_s(g_Message, MAX_PATH, message);
	g_MessageLength = strlen(message);
	CreateThread(0, 0, ShowInternal, 0, 0, 0);
}

bool Timeout::UserCancelled()
{
	return g_Shutdown;
}

void Timeout::Cleanup()
{
	g_Shutdown = true;
}

void Timeout::Draw()
{
	PAINTSTRUCT ps;
	HDC hDC = BeginPaint(g_hWnd, &ps);

	// White background
	FillRect(hDC, &g_Fill, (HBRUSH) GetStockObject(WHITE_BRUSH));

	// Spinner
    HDC hMemDC = CreateCompatibleDC(hDC);
    HBITMAP hOldBmp = (HBITMAP) SelectObject(hMemDC, g_SpinBitmaps[g_CurrentImage]);   
	BitBlt(hDC, 10, 11, IMG_SIZE, IMG_SIZE, hMemDC, 0, 0, SRCCOPY);
    SelectObject(hMemDC, hOldBmp);
	DeleteDC(hMemDC);
	g_CurrentImage++;
	if(g_CurrentImage >= 12) g_CurrentImage = 0;

	// Function text
	HFONT old = (HFONT) SelectObject(hDC, g_Font);
	SetTextColor(hDC, RGB(80, 80, 80));
	TextOut(hDC, 57, 14, g_Message, g_MessageLength);
	SetTextColor(hDC, RGB(180, 180, 180));
	TextOut(hDC, 57, 31, CLICK_MESSAGE, CLICK_MESSAGE_LEN);
	SelectObject(hDC, old);

	EndPaint(g_hWnd, &ps);	
}

void Timeout::Initialise(HINSTANCE hInstance)
{
	RegisterWindowClass(hInstance);
	LoadBitmaps(hInstance);

	// Create our font
	HDC hDC = GetDC(NULL);
	DWORD h = -MulDiv(10, GetDeviceCaps(hDC, LOGPIXELSY), 72);
	g_Font = CreateFont(h, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "Arial");
	g_hInstance = hInstance;
	g_Initialised = true;
}

void Timeout::RegisterWindowClass(HINSTANCE hInstance)
{
	WNDCLASSEX wcx;
	wcx.cbSize = sizeof(wcx);
	wcx.style = CS_BYTEALIGNCLIENT | CS_BYTEALIGNWINDOW;
	wcx.lpfnWndProc = MainWndProc;
	wcx.cbClsExtra = 0;
	wcx.cbWndExtra = DLGWINDOWEXTRA;
	wcx.hInstance = 0;
	wcx.hIcon = 0;
	wcx.hCursor = LoadCursor(NULL, IDC_APPSTARTING);
	wcx.hbrBackground = (HBRUSH) GetStockObject(LTGRAY_BRUSH);
	wcx.lpszMenuName = 0;
	wcx.lpszClassName = XLLOOP_WND_CLASS;
	wcx.hIconSm = 0;
	RegisterClassEx(&wcx);
}

void Timeout::LoadBitmaps(HINSTANCE hInstance)
{
	for(int i =1; i <= 12; i++) {
		HRSRC hi = FindResource(hInstance, MAKEINTRESOURCE(i), MAKEINTRESOURCE(689));
		HGLOBAL hgbl = LoadResource(hInstance, hi);
		DWORD size = SizeofResource(hInstance, hi);
		LPVOID data = GlobalLock(hgbl);
		HGLOBAL hcopy = GlobalAlloc(GMEM_MOVEABLE, size);
		LPVOID pcopy = GlobalLock(hcopy);
		memcpy(pcopy, data, size);
		GlobalUnlock(hcopy);
		g_SpinBitmaps[i - 1] = LoadImageBitmap(pcopy, size);
		GlobalUnlock(pcopy);
		GlobalFree(pcopy);
	}	
}

HBITMAP Timeout::LoadImageBitmap(HGLOBAL hgbl, DWORD size)
{
	HBITMAP hbmp = NULL;
	CoInitialize(NULL);
	IStream* stream;
	HRESULT hr = CreateStreamOnHGlobal(hgbl, FALSE, &stream);
	if(SUCCEEDED(hr) && stream) {
		ULARGE_INTEGER ul;
		ul.LowPart = size;
		ul.HighPart = 0;
		stream->SetSize(ul);
		IPicture* picture;
		// Load picture from stream
		hr = OleLoadPicture(stream, 0, 0, IID_IPicture, (void**)&picture);
		if(SUCCEEDED(hr) && picture) {
			// Copy picture to a bitmap resource
			HBITMAP hsrc;                
			picture->get_Handle((OLE_HANDLE *)&hsrc);
			hbmp = (HBITMAP)CopyImage(hsrc, IMAGE_BITMAP, 0, 0, 0);
			picture->Release();
		}
		stream->Release();
	}
	CoUninitialize();
	return hbmp;
}

LRESULT CALLBACK MainWndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch(uMsg) {
	case WM_PAINT:
		Timeout::Draw();
		break;
	case WM_LBUTTONDOWN:
		Timeout::Cleanup();
		break;
	}
	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}

