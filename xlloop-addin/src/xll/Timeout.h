/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef TIMEOUT_H
#define TIMEOUT_H

#include "../common/Runtime.h"
#include "../common/INI.h"

class Timeout 
{
public:
	static void Init();
	static void Show(const WCHAR* function);
	static bool UserCancelled();
	static void Cleanup();
	static void Draw();
	static void Initialise(HINSTANCE hInstance, dictionary* ini);

private:
	static DWORD WINAPI ShowInternal(LPVOID);
	static void RegisterWindowClass(HINSTANCE hInstance);
	static void LoadBitmaps(HINSTANCE hInstance);
	static HBITMAP LoadImageBitmap(HGLOBAL hgbl, DWORD size);

private:
};

#endif // TIMEOUT_H