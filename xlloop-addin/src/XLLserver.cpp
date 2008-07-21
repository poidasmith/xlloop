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
#include "common/INI.h"
#include "common/Log.h"

void FindXLLs(dictionary* ini, char** result, int* current, int max)
{
	char search[MAX_PATH];
	char* moduleDir = iniparser_getstr(ini, MODULE_DIR);
	search[0] = 0;
	strcat(search, moduleDir);
	strcat(search, "\\*.xll");
	WIN32_FIND_DATA fd;
	HANDLE h = FindFirstFile(search, &fd);
	char fullpath[MAX_PATH];
	if(h != INVALID_HANDLE_VALUE) {
		strcpy(fullpath, moduleDir);
		strcat(fullpath, "\\");
		strcat(fullpath, fd.cFileName);
		result[(*current)++] = strdup(fullpath);
		while(FindNextFile(h, &fd) != 0) {
			strcpy(fullpath, moduleDir);
			strcat(fullpath, "\\");
			strcat(fullpath, fd.cFileName);
			result[(*current)++] = strdup(fullpath);
		}
	}
}

typedef struct _FunctionInfo {
	HMODULE dll;
	char* functionDef;
	char* functionName;
} FunctionInfo;

void AddXLL(char* filename)
{
	HMODULE dll = LoadLibrary(filename);
	DWORD d = GetLastError();
	typedef int (CALLBACK* XLAUTOOPENFN)();
	XLAUTOOPENFN xlAutoOpen = (XLAUTOOPENFN) GetProcAddress(dll, "xlAutoOpen");
	if(xlAutoOpen)
		xlAutoOpen();
}

void printOper(LPXLOPER lpx)
{
	static char temp[MAX_PATH];
	printf("type: %d ", lpx->xltype);
	switch(lpx->xltype & ~(xlbitXLFree | xlbitDLLFree))
	{
	case xltypeStr:
		strncpy(temp, lpx->val.str + 1, lpx->val.str[0]);
		temp[lpx->val.str[0]] = 0;
		printf("str: %s", temp);
	default:
		break;
	}
}

int far pascal Excel4v(int xlfn, LPXLOPER operRes, int count, LPXLOPER far opers[])
{
	printf("Excel4v: %d %d\n", xlfn, count);
	if(xlfn == 149) {
		for(int i = 0; i < count; i++) {
			printOper(opers[i]);
		}
	}
	return 0;
}

int far pascal XLCallVer(void)
{
	return 4;
}

int main(int argc, char* argv[])
{
	HINSTANCE hInstance = (HINSTANCE) GetModuleHandle(NULL);
	Log::Init(hInstance, NULL, NULL);
	dictionary* ini = INI::LoadIniFile(hInstance);

	if(ini == NULL) {
		Log::Error("Module INI file not found\n");
		return 1;
	}

	// Load stub library
	HMODULE stubModule = LoadLibrary("XLLstub-Debug\\xlcall32.dll");
	if(stubModule == NULL) {
		Log::Error("Could not load stub library\n");
		return 1;
	}

	// Attach function pointers
	typedef void (CALLBACK* LPFNDLLFUNC1)(void**,void**);
	LPFNDLLFUNC1 SetFP = (LPFNDLLFUNC1) GetProcAddress(stubModule, "SetFunctionPointers");
	SetFP((void**)Excel4v, (void**)XLCallVer);

	// Find the XLLs in the search directory and add them
	
	char* entries[20];
	int numEntries = 0;
	FindXLLs(ini, entries, &numEntries, 20);
	for(int i = 0; i < numEntries; i++) {
		printf("%s\n", entries[i]);
		AddXLL(entries[i]);
	}


	AddXLL("F:\\Downloads\\xllsdk97\\SAMPLES\\FRAMEWRK\\generic.xll");
	
	return 0;
}