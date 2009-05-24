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
#include <stdio.h>
#include "xlcall.h"

#define XLLOOP_WND_CLASS "XLLoop.BusyClass"
#define XLLOOP_WND_NAME "XLLoop.BusyWindow"
#define CLICK_MESSAGE "click to cancel"
#define CLICK_MESSAGE_LEN 15
#define IMG_SIZE 40

#define DISABLE_OPTION ":disable.calc.popup"
#define DISABLE_CANCEL ":disable.calc.cancel"

namespace 
{
	bool g_Initialised = false;
	bool g_CanCancel = true;
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
	DWORD x = r.left + (r.right - r.left)/2 - g_Width/2;
	DWORD y = r.top + (r.bottom - r.top)/2 - g_Height/2;
	g_hWnd = CreateWindowEx(
		WS_EX_TOOLWINDOW | WS_EX_NOPARENTNOTIFY, 
		XLLOOP_WND_CLASS, 
		XLLOOP_WND_NAME, 
		WS_POPUP | WS_VISIBLE, 
		x, y,
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

	// Force parent to repaint the area
	r.left = x;
	r.top = y;
	r.right = x + g_Width;
	r.bottom = y + g_Height;
	InvalidateRect(g_Parent, &r, TRUE);
	UpdateWindow(g_Parent);

	return 0;
}

void Timeout::Init()
{
	g_CurrentImage = 0;
	g_Shutdown = false;
}

typedef struct
{
	unsigned short lo;
	HWND hwnd;
} WindowInfo;

BOOL CALLBACK EnumProc(HWND hwnd, WindowInfo* pInfo)
{
	char buf[MAX_PATH];
	GetClassName(hwnd, buf, MAX_PATH);
	if(strcmp(buf, "XLMAIN") == 0) {
		if(LOWORD((DWORD) hwnd) == pInfo->lo) {
			pInfo->hwnd = hwnd;
			return FALSE;
		}
	}

	return TRUE;
}

void Timeout::Show(const char* function)
{
	if(!g_Initialised) return;
	if(g_Started) return;

	// Find the excel hwnd
	XLOPER x;
	Excel4(xlGetHwnd, &x, 0);
	WindowInfo info;
	info.lo = x.val.w;
	EnumWindows((WNDENUMPROC) EnumProc, (LPARAM) &info);

	// Find the active cell
	Excel4(xlfCaller, &x, 0);

	// Now format the message
	if(x.val.sref.ref.rwLast > x.val.sref.ref.rwFirst) {
		sprintf(g_Message, "Calculating %c%d:%c%d: %s()", x.val.sref.ref.colFirst + 'A', 
			x.val.sref.ref.rwFirst + 1, x.val.sref.ref.colLast + 'A', x.val.sref.ref.rwLast + 1, function);
	} else {
		sprintf(g_Message, "Calculating %c%d: %s()", x.val.sref.ref.colFirst + 'A', 
			x.val.sref.ref.rwFirst + 1, function);
	}

	g_CurrentImage = 0;
	g_Shutdown = false;
	g_Parent = info.hwnd;
	g_MessageLength = strlen(g_Message);
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
	if(g_CanCancel) {
		TextOut(hDC, 57, 14, g_Message, g_MessageLength);
		SetTextColor(hDC, RGB(180, 180, 180));
		TextOut(hDC, 57, 31, CLICK_MESSAGE, CLICK_MESSAGE_LEN);
	} else {
		TextOut(hDC, 57, 22, g_Message, g_MessageLength);
	}
	SelectObject(hDC, old);

	EndPaint(g_hWnd, &ps);	
}

void Timeout::Initialise(HINSTANCE hInstance, dictionary* ini)
{
	// Check if the user wants this feature
	bool disable = iniparser_getboolean(ini, DISABLE_OPTION, false);
	if(disable) return;
	g_CanCancel = !iniparser_getboolean(ini, DISABLE_CANCEL, false);

	// Do some background stuff
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
		if(g_CanCancel)
			Timeout::Cleanup();
		break;
	}
	return DefWindowProc(hWnd, uMsg, wParam, lParam);
}

