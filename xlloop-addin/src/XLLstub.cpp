/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "common/Runtime.h"
#include "xll/xlcall.h"

// The DLL instance
static HINSTANCE g_hinstance = NULL;
static void** g_excel4v = NULL;
static void** g_xlcallver = NULL;

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
	if(fdwReason == DLL_PROCESS_ATTACH) {
		// Store reference to handle for later use
		g_hinstance = hinstDLL;
	}

	// OK
	return 1;
}

#ifdef __cplusplus
extern "C" {  
#endif 

int far pascal XLCallVer(void)
{
	typedef int (CALLBACK* XLCALLVER)(void);
	return ((XLCALLVER)g_xlcallver)();
}

int far _cdecl Excel4(int xlfn, LPXLOPER operRes, int count,... )
{
	va_list argptr;
	va_start(argptr, count);
	LPXLOPER* opers = new LPXLOPER[count];
	for(int i = 0; i < count; i++) {
		opers[i] = va_arg(argptr, LPXLOPER);
	}
	int res = Excel4v(xlfn, operRes, count, opers);
	delete [] opers;
	return res;
}

int far pascal Excel4v(int xlfn, LPXLOPER operRes, int count, LPXLOPER far opers[])
{
	typedef int (CALLBACK* EXCEL4VFN)(int xlfn, LPXLOPER operRes, int count, LPXLOPER far opers[]);
	return ((EXCEL4VFN)g_excel4v)(xlfn, operRes, count, opers);
}

int far _cdecl LPenHelper(void)
{
	return 0;
}

void _cdecl SetFunctionPointers(void** Excel4v, void** XLCallVer)
{
	g_excel4v = Excel4v;
	g_xlcallver = XLCallVer;
}

#ifdef __cplusplus
}
#endif