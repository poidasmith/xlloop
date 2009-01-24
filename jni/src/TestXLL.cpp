/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "TestXLL.h"
#include <stdio.h>

LPSTR MakeExcelString(const char* string)
{
	if(string == NULL) return NULL;
	size_t len = strlen(string);
	if(len > 255) len = 255; // Excel strings are limited to 255 chars
	char* temp = (char *) malloc(len + 2);
	memcpy(temp + 1, string, len);
	temp[0] = (BYTE) len;
	return temp;
}

// A helper function used to register a function
int RegisterFunction(LPXLOPER xllName, 
					  const char* procedure, const char* typeText, const char* functionText,
					  const char* argumentText, const char* macroType, const char* category,
					  const char* shortcutText, const char* helpTopic, 
					  const char* functionHelp, const char* argumentHelp, bool command)
{
	static XLOPER args[10];
	for(int i = 0; i < 10; i++) {
		args[i].val.str = NULL;
		args[i].xltype = xltypeStr;
	}
	args[0].val.str = MakeExcelString(procedure);
	args[1].val.str = MakeExcelString(typeText);
	args[2].val.str = MakeExcelString(functionText);
	args[3].val.str = MakeExcelString(argumentText);
	args[4].val.str = MakeExcelString(macroType);
	args[5].val.str = MakeExcelString(category);
	args[6].val.str = MakeExcelString(shortcutText);
	if(!command) {
		args[7].val.str = MakeExcelString(helpTopic);
		args[8].val.str = MakeExcelString(functionHelp);
		args[9].val.str = MakeExcelString(argumentHelp);
	}

	// Check types for NULL
	for(int i = 0; i < 10; i++) {
		if(args[i].val.str == NULL)
			args[i].xltype = xltypeMissing;
	}

	int res = 0;
	if(!command) {
		res = Excel4(xlfRegister, 0, 11, (LPXLOPER) xllName, (LPXLOPER) &args[0], 
			(LPXLOPER) &args[1], (LPXLOPER) &args[2], (LPXLOPER) &args[3],
			(LPXLOPER) &args[4], (LPXLOPER) &args[5], (LPXLOPER) &args[6], 
			(LPXLOPER) &args[7], (LPXLOPER) &args[8], (LPXLOPER) &args[9]);
	} else {
		res = Excel4(xlfRegister, 0, 8, (LPXLOPER) xllName, (LPXLOPER) &args[0], 
			(LPXLOPER) &args[1], (LPXLOPER) &args[2], (LPXLOPER) &args[3],
			(LPXLOPER) &args[4], (LPXLOPER) &args[5], (LPXLOPER) &args[6]);
	}

	if(res != 0) {
		printf("Failed to register %s\n", procedure);
	}

	// Free strings
	for(int i = 0; i < 10; i++) {
		if(!args[i].val.str == NULL)
			free(args[i].val.str);
	}

	return res;
}


#ifdef __cplusplus
extern "C" {  
#endif 

__declspec(dllexport) int WINAPI xlAutoOpen(void)
{
	XLCallVer();

	static XLOPER xDLL;
	Excel4(xlGetName, &xDLL, 0);

	RegisterFunction(&xDLL, "TestSum", "BBB", "TestSum", 
		NULL, "1", "General", NULL, NULL, NULL, NULL, false);

	RegisterFunction(&xDLL, "TestChar", "CRFPR", "TestChar", 
		NULL, "1", "General", NULL, NULL, NULL, NULL, false);

	RegisterFunction(&xDLL, "TestArgs", "PPDAJI", "TestArgs", 
		NULL, "1", "General", NULL, NULL, NULL, NULL, false);

	return 1;
}

__declspec(dllexport) double WINAPI TestSum(double a, double b)
{
	return a+b;
}

__declspec(dllexport) char* WINAPI TestChar(LPXLOPER a1, char* a2, LPXLOPER a3, LPXLOPER a4)
{
	return "Hello Function";
}

__declspec(dllexport) LPXLOPER WINAPI TestArgs(LPXLOPER a1, char* a2, bool a3, int a4, short a5)
{
	static XLOPER xRes;
	xRes.xltype = xltypeStr;
	xRes.val.str = a2;
	
	return (LPXLOPER) &xRes;
}

__declspec(dllexport) int WINAPI xlAutoClose(void)
{
	return 1;
}

__declspec(dllexport) LPXLOPER WINAPI xlAutoRegister(LPXLOPER pxName)
{
	static XLOPER xDLL, xRegId;
	xRegId.xltype = xltypeErr;
	xRegId.val.err = xlerrValue;
	
	return (LPXLOPER) &xRegId;
}

__declspec(dllexport) int WINAPI xlAutoAdd(void)
{
	return 1;
}

__declspec(dllexport) int WINAPI xlAutoRemove(void)
{
	return 1;
}

__declspec(dllexport) void WINAPI xlAutoFree(LPXLOPER px)
{
}

__declspec(dllexport) LPXLOPER WINAPI xlAddInManagerInfo(LPXLOPER xAction)
{
	static XLOPER xInfo, xIntAction, xIntType;
	xIntType.xltype = xltypeInt;
	xIntType.val.w = xltypeInt;
	xInfo.xltype = xltypeErr;
	xInfo.val.err = xlerrValue;

	return (LPXLOPER) &xInfo;
}

#ifdef __cplusplus
}
#endif