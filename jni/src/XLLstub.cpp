/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#define WIN32_LEAN_MEAN
#include <windows.h>
#include "xlcall.h"

typedef int (CALLBACK* EXCEL4VFN)(int xlfn, LPXLOPER operRes, int count, LPXLOPER far opers[]);
static EXCEL4VFN EXCEL4V = NULL;

typedef int (CALLBACK* XLCALLVERFN)(void);
static XLCALLVERFN XLCALLVER = NULL;

#ifdef __cplusplus
extern "C" {  
#endif 

int far pascal XLCallVer(void)
{
	return XLCALLVER();
}

int far _cdecl Excel4(int xlfn, LPXLOPER operRes, int count,... )
{
	va_list argptr;
	va_start(argptr, count);
	LPXLOPER* opers = new LPXLOPER[count];
	for(int i = 0; i < count; i++) {
		opers[i] = va_arg(argptr, LPXLOPER);
	}
	int res = EXCEL4V(xlfn, operRes, count, opers);
	delete [] opers;
	return res;
}

int far pascal Excel4v(int xlfn, LPXLOPER operRes, int count, LPXLOPER far opers[])
{
	return EXCEL4V(xlfn, operRes, count, opers);
}

int far _cdecl LPenHelper(void)
{
	return 0;
}

void _cdecl SetFunctionPointers(void** Excel4v, void** XLCallVer)
{
	EXCEL4V = (EXCEL4VFN) Excel4v;
	XLCALLVER = (XLCALLVERFN) XLCallVer;
}

#ifdef __cplusplus
}
#endif