/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "common/INI.h"
#include "common/Log.h"
#include "java/VM.h"
#include "java/JNI.h"
#include "java/Classpath.h"
#include "xll/xlcall.h"
#include "xll/XLUtil.h"
#include "xll/XLObject.h"
#include "xll/XLObjectFactory.h"
#include "xll/XLAddin.h"
#include <vector>
#include <varargs.h>
#include <jni.h>

using namespace std;

#define MAX_ARGS 30
#define ADDIN_CLASS ":addin.class"

// The DLL instance
static HINSTANCE g_hinstance;

// The INI file
static dictionary* g_ini = NULL;

// These are used to provide a diagnosis of any error. Each module has a function [ModuleName]_GetLastError().
static char g_modulename[MAX_PATH];

// The addin/function implementations
static XLAddin g_addin;

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
	if(fdwReason == DLL_PROCESS_ATTACH)
	{
		g_hinstance = hinstDLL;

		// Store a reference to the module filename
		char filename[MAX_PATH];
		::GetModuleFileName(g_hinstance, filename, MAX_PATH);

		// Now extract just the module name
		char mname[MAX_PATH];
		strcpy_s(mname, MAX_PATH, filename);
		size_t len = strlen(mname);
		mname[len - 4] = 0;
		for(size_t i = len - 4; i >= 0; i--) {
			if(mname[i] == '\\' || mname[i] == '/') {
				strcpy_s(g_modulename, MAX_PATH, &mname[i+1]);
				break;
			}
		}
	}

	if(fdwReason = DLL_PROCESS_DETACH)
	{
	}

	return TRUE;
}

bool StartupVM()
{
	// Attempt to load the ini file for this module
	g_ini = INI::LoadIniFile(g_hinstance);

	if(g_ini == NULL) {
		Log::SetLastError("Could not find module ini file");
		return false;
	}

	// Attempt to find an appropriate java VM
	char* vmlibrary = VM::FindJavaVMLibrary(g_ini);
	if(!vmlibrary) {
		Log::SetLastError("Could not find VM");
		return false;
	}

	// Collect the VM args from the INI file
	TCHAR *vmargs[MAX_PATH];
	int vmargsCount = 0;
	INI::GetNumberedKeysFromIni(g_ini, VM_ARG, vmargs, vmargsCount);

	// Build up the classpath and add to vm args
	Classpath::BuildClassPath(g_ini, vmargs, vmargsCount);

	// Extract the specific VM args
	VM::ExtractSpecificVMArgs(g_ini, vmargs, vmargsCount);

	// Make sure there is a NULL at the end of the args
	vmargs[vmargsCount] = NULL;

	// Fire up the VM
	int vmerr = VM::StartJavaVM(vmlibrary, vmargs);
	if(vmerr != 0) {
		Log::SetLastError("VM could not be started (returned %d)", vmerr);
		return false;
	}

	// Free vm args
	for(int i = 0; i < vmargsCount; i++) {
		free(vmargs[i]);
	}

	return true;
}

bool Initialize()
{
	bool result = StartupVM();
	if(!result) {
		return result;
	}

	JNIEnv* env = VM::GetJNIEnv();
	result = INI::RegisterNatives(env);
	if(!result) {
		return result;
	}

	result = XLUtil::RegisterNatives(env, g_modulename);
	if(!result) {
		return result;
	}

	result = XLObjectFactory::RegisterNatives(env);
	if(!result) {
		return result;
	}

	result = XLObject::RegisterNatives(env);
	if(!result) {
		return result;
	}

	result = g_addin.Load(env, iniparser_getstr(g_ini, ADDIN_CLASS));
	if(!result) {
		return result;
	}
	
	return true;
}

#ifdef __cplusplus
extern "C" {  
#endif 

__declspec(dllexport) int WINAPI xlAutoOpen(void)
{
	static XLOPER xDLL;
	Excel4(xlGetName, &xDLL, 0);

	// Register function for retrieving error text
	char errorFunction[MAX_PATH];
	sprintf_s(errorFunction, MAX_PATH, "%s_GetLastError", g_modulename);
	int res = XLUtil::RegisterFunction(&xDLL, "EA4JGetLastError", "R!", errorFunction, 
		NULL, "1", "Information", NULL, NULL, NULL, NULL);

	// Fire up the VM
	bool initRes = Initialize();
	
	// Register java functions (if VM started ok)
	if(initRes) {
		char javaFunction[MAX_PATH];
		JNIEnv* env = VM::GetJNIEnv();
		for(int i = 0; i < g_addin.GetNumFunctions(); i++) {
			const XLFunction& func = g_addin.GetFunction(i);
			sprintf_s(javaFunction, MAX_PATH, "EA4JFunc%d", (i + 1));
			const char* typeText = func.isVolatile() ? "RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR!" : "RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR";
			XLUtil::RegisterFunction(&xDLL, javaFunction, typeText,
				func.GetFunctionText(), func.GetArgumentText(), func.GetMacroType(),
				func.GetCategory(), func.GetShortcutText(), func.GetHelpTopic(),
				func.GetFunctionHelp(), func.GetArgumentHelp());
		}
	}

	// Free the XLL filename
	Excel4(xlFree, 0, 1, (LPXLOPER) &xDLL);

	// OK
	return 1;
}

__declspec(dllexport) int WINAPI xlAutoClose(void)
{
	// Destroy VM (note that this may block if there are non-daemon threads created in the VM)
	VM::CleanupVM();

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

__declspec(dllexport) LPXLOPER WINAPI xlAddInManagerInfo(LPXLOPER xAction)
{
	static XLOPER xInfo, xIntAction, xIntType;
	xIntType.xltype = xltypeInt;
	xIntType.val.w = xltypeInt;

	Excel4(xlCoerce, &xIntAction, 2, xAction, &xIntType);

	if(xIntAction.val.w == 1) {
		xInfo.xltype = xltypeStr | xlbitXLFree;
		xInfo.val.str = XLUtil::MakeExcelString(g_addin.GetLongName().c_str());
	} else {
		xInfo.xltype = xltypeErr;
		xInfo.val.err = xlerrValue;
	}

	return (LPXLOPER) &xInfo;
}

__declspec(dllexport) LPXLOPER WINAPI EA4JGetLastError(LPXLOPER args)
{
	static XLOPER err;
	static char errStr[MAX_PATH];
	const char* error = Log::GetLastError();
	sprintf_s(errStr, MAX_PATH, " %s", error != NULL ? error : "OK");
	errStr[0] = (BYTE) lstrlen(errStr+1);
	err.xltype = xltypeStr;
	err.val.str = errStr;
	return &err;
}

#define DECLARE_EXCEL_FUNCTION(number) \
__declspec(dllexport) LPXLOPER WINAPI EA4JFunc##number (LPXLOPER px1, LPXLOPER px2, LPXLOPER px3 \
	,LPXLOPER px4, LPXLOPER px5, LPXLOPER px6, LPXLOPER px7, LPXLOPER px8, LPXLOPER px9 \
	,LPXLOPER px10, LPXLOPER px11, LPXLOPER px12, LPXLOPER px13, LPXLOPER px14, LPXLOPER px15 \
	,LPXLOPER px16, LPXLOPER px17, LPXLOPER px18, LPXLOPER px19, LPXLOPER px20, LPXLOPER px21 \
	,LPXLOPER px22, LPXLOPER px23, LPXLOPER px24, LPXLOPER px25, LPXLOPER px26, LPXLOPER px27\
	,LPXLOPER px28, LPXLOPER px29, LPXLOPER px30) \
{ \
	LPXLOPER args[30]; \
	args[0] = px1; \
	args[1] = px2; \
	args[2] = px3; \
	args[3] = px4; \
	args[4] = px5; \
	args[5] = px6; \
	args[6] = px7; \
	args[7] = px8; \
	args[8] = px9; \
	args[9] = px10; \
	args[10] = px11; \
	args[11] = px12; \
	args[12] = px13; \
	args[13] = px14; \
	args[14] = px15; \
	args[15] = px16; \
	args[16] = px17; \
	args[17] = px18; \
	args[18] = px19; \
	args[19] = px20; \
	args[20] = px21; \
	args[21] = px22; \
	args[22] = px23; \
	args[23] = px24; \
	args[24] = px25; \
	args[25] = px26; \
	args[26] = px27; \
	args[27] = px28; \
	args[28] = px29; \
	args[29] = px30; \
	int missingCount = 0; \
	for(; missingCount < 30; missingCount++) { \
		if(args[missingCount]->xltype == xltypeMissing) break; \
	} \
	return g_addin.GetFunction(number - 1).Execute(missingCount, args); \
} \

DECLARE_EXCEL_FUNCTION(1)
DECLARE_EXCEL_FUNCTION(2)
DECLARE_EXCEL_FUNCTION(3)
DECLARE_EXCEL_FUNCTION(4)
DECLARE_EXCEL_FUNCTION(5)
DECLARE_EXCEL_FUNCTION(6)
DECLARE_EXCEL_FUNCTION(7)
DECLARE_EXCEL_FUNCTION(8)
DECLARE_EXCEL_FUNCTION(9)
DECLARE_EXCEL_FUNCTION(10)
DECLARE_EXCEL_FUNCTION(11)
DECLARE_EXCEL_FUNCTION(12)
DECLARE_EXCEL_FUNCTION(13)
DECLARE_EXCEL_FUNCTION(14)
DECLARE_EXCEL_FUNCTION(15)
DECLARE_EXCEL_FUNCTION(16)
DECLARE_EXCEL_FUNCTION(17)
DECLARE_EXCEL_FUNCTION(18)
DECLARE_EXCEL_FUNCTION(19)
DECLARE_EXCEL_FUNCTION(20)
DECLARE_EXCEL_FUNCTION(21)
DECLARE_EXCEL_FUNCTION(22)
DECLARE_EXCEL_FUNCTION(23)
DECLARE_EXCEL_FUNCTION(24)
DECLARE_EXCEL_FUNCTION(25)
DECLARE_EXCEL_FUNCTION(26)
DECLARE_EXCEL_FUNCTION(27)
DECLARE_EXCEL_FUNCTION(28)
DECLARE_EXCEL_FUNCTION(29)
DECLARE_EXCEL_FUNCTION(30)
DECLARE_EXCEL_FUNCTION(31)
DECLARE_EXCEL_FUNCTION(32)
DECLARE_EXCEL_FUNCTION(33)
DECLARE_EXCEL_FUNCTION(34)
DECLARE_EXCEL_FUNCTION(35)
DECLARE_EXCEL_FUNCTION(36)
DECLARE_EXCEL_FUNCTION(37)
DECLARE_EXCEL_FUNCTION(38)
DECLARE_EXCEL_FUNCTION(39)
DECLARE_EXCEL_FUNCTION(40)
DECLARE_EXCEL_FUNCTION(41)
DECLARE_EXCEL_FUNCTION(42)
DECLARE_EXCEL_FUNCTION(43)
DECLARE_EXCEL_FUNCTION(44)
DECLARE_EXCEL_FUNCTION(45)
DECLARE_EXCEL_FUNCTION(46)
DECLARE_EXCEL_FUNCTION(47)
DECLARE_EXCEL_FUNCTION(48)
DECLARE_EXCEL_FUNCTION(49)
DECLARE_EXCEL_FUNCTION(50)
DECLARE_EXCEL_FUNCTION(51)
DECLARE_EXCEL_FUNCTION(52)
DECLARE_EXCEL_FUNCTION(53)
DECLARE_EXCEL_FUNCTION(54)
DECLARE_EXCEL_FUNCTION(55)
DECLARE_EXCEL_FUNCTION(56)
DECLARE_EXCEL_FUNCTION(57)
DECLARE_EXCEL_FUNCTION(58)
DECLARE_EXCEL_FUNCTION(59)
DECLARE_EXCEL_FUNCTION(60)
DECLARE_EXCEL_FUNCTION(61)
DECLARE_EXCEL_FUNCTION(62)
DECLARE_EXCEL_FUNCTION(63)
DECLARE_EXCEL_FUNCTION(64)
DECLARE_EXCEL_FUNCTION(65)
DECLARE_EXCEL_FUNCTION(66)
DECLARE_EXCEL_FUNCTION(67)
DECLARE_EXCEL_FUNCTION(68)
DECLARE_EXCEL_FUNCTION(69)
DECLARE_EXCEL_FUNCTION(70)
DECLARE_EXCEL_FUNCTION(71)
DECLARE_EXCEL_FUNCTION(72)
DECLARE_EXCEL_FUNCTION(73)
DECLARE_EXCEL_FUNCTION(74)
DECLARE_EXCEL_FUNCTION(75)
DECLARE_EXCEL_FUNCTION(76)
DECLARE_EXCEL_FUNCTION(77)
DECLARE_EXCEL_FUNCTION(78)
DECLARE_EXCEL_FUNCTION(79)
DECLARE_EXCEL_FUNCTION(80)
DECLARE_EXCEL_FUNCTION(81)
DECLARE_EXCEL_FUNCTION(82)
DECLARE_EXCEL_FUNCTION(83)
DECLARE_EXCEL_FUNCTION(84)
DECLARE_EXCEL_FUNCTION(85)
DECLARE_EXCEL_FUNCTION(86)
DECLARE_EXCEL_FUNCTION(87)
DECLARE_EXCEL_FUNCTION(88)
DECLARE_EXCEL_FUNCTION(89)
DECLARE_EXCEL_FUNCTION(90)
DECLARE_EXCEL_FUNCTION(91)
DECLARE_EXCEL_FUNCTION(92)
DECLARE_EXCEL_FUNCTION(93)
DECLARE_EXCEL_FUNCTION(94)
DECLARE_EXCEL_FUNCTION(95)
DECLARE_EXCEL_FUNCTION(96)
DECLARE_EXCEL_FUNCTION(97)
DECLARE_EXCEL_FUNCTION(98)
DECLARE_EXCEL_FUNCTION(99)
DECLARE_EXCEL_FUNCTION(100)
DECLARE_EXCEL_FUNCTION(101)
DECLARE_EXCEL_FUNCTION(102)
DECLARE_EXCEL_FUNCTION(103)
DECLARE_EXCEL_FUNCTION(104)
DECLARE_EXCEL_FUNCTION(105)
DECLARE_EXCEL_FUNCTION(106)
DECLARE_EXCEL_FUNCTION(107)
DECLARE_EXCEL_FUNCTION(108)
DECLARE_EXCEL_FUNCTION(109)
DECLARE_EXCEL_FUNCTION(110)
DECLARE_EXCEL_FUNCTION(111)
DECLARE_EXCEL_FUNCTION(112)
DECLARE_EXCEL_FUNCTION(113)
DECLARE_EXCEL_FUNCTION(114)
DECLARE_EXCEL_FUNCTION(115)
DECLARE_EXCEL_FUNCTION(116)
DECLARE_EXCEL_FUNCTION(117)
DECLARE_EXCEL_FUNCTION(118)
DECLARE_EXCEL_FUNCTION(119)
DECLARE_EXCEL_FUNCTION(120)
DECLARE_EXCEL_FUNCTION(121)
DECLARE_EXCEL_FUNCTION(122)
DECLARE_EXCEL_FUNCTION(123)
DECLARE_EXCEL_FUNCTION(124)
DECLARE_EXCEL_FUNCTION(125)
DECLARE_EXCEL_FUNCTION(126)
DECLARE_EXCEL_FUNCTION(127)
DECLARE_EXCEL_FUNCTION(128)
DECLARE_EXCEL_FUNCTION(129)
DECLARE_EXCEL_FUNCTION(130)
DECLARE_EXCEL_FUNCTION(131)
DECLARE_EXCEL_FUNCTION(132)
DECLARE_EXCEL_FUNCTION(133)
DECLARE_EXCEL_FUNCTION(134)
DECLARE_EXCEL_FUNCTION(135)
DECLARE_EXCEL_FUNCTION(136)
DECLARE_EXCEL_FUNCTION(137)
DECLARE_EXCEL_FUNCTION(138)
DECLARE_EXCEL_FUNCTION(139)
DECLARE_EXCEL_FUNCTION(140)
DECLARE_EXCEL_FUNCTION(141)
DECLARE_EXCEL_FUNCTION(142)
DECLARE_EXCEL_FUNCTION(143)
DECLARE_EXCEL_FUNCTION(144)
DECLARE_EXCEL_FUNCTION(145)
DECLARE_EXCEL_FUNCTION(146)
DECLARE_EXCEL_FUNCTION(147)
DECLARE_EXCEL_FUNCTION(148)
DECLARE_EXCEL_FUNCTION(149)
DECLARE_EXCEL_FUNCTION(150)
DECLARE_EXCEL_FUNCTION(151)
DECLARE_EXCEL_FUNCTION(152)
DECLARE_EXCEL_FUNCTION(153)
DECLARE_EXCEL_FUNCTION(154)
DECLARE_EXCEL_FUNCTION(155)
DECLARE_EXCEL_FUNCTION(156)
DECLARE_EXCEL_FUNCTION(157)
DECLARE_EXCEL_FUNCTION(158)
DECLARE_EXCEL_FUNCTION(159)
DECLARE_EXCEL_FUNCTION(160)
DECLARE_EXCEL_FUNCTION(161)
DECLARE_EXCEL_FUNCTION(162)
DECLARE_EXCEL_FUNCTION(163)
DECLARE_EXCEL_FUNCTION(164)
DECLARE_EXCEL_FUNCTION(165)
DECLARE_EXCEL_FUNCTION(166)
DECLARE_EXCEL_FUNCTION(167)
DECLARE_EXCEL_FUNCTION(168)
DECLARE_EXCEL_FUNCTION(169)
DECLARE_EXCEL_FUNCTION(170)
DECLARE_EXCEL_FUNCTION(171)
DECLARE_EXCEL_FUNCTION(172)
DECLARE_EXCEL_FUNCTION(173)
DECLARE_EXCEL_FUNCTION(174)
DECLARE_EXCEL_FUNCTION(175)
DECLARE_EXCEL_FUNCTION(176)
DECLARE_EXCEL_FUNCTION(177)
DECLARE_EXCEL_FUNCTION(178)
DECLARE_EXCEL_FUNCTION(179)
DECLARE_EXCEL_FUNCTION(180)
DECLARE_EXCEL_FUNCTION(181)
DECLARE_EXCEL_FUNCTION(182)
DECLARE_EXCEL_FUNCTION(183)
DECLARE_EXCEL_FUNCTION(184)
DECLARE_EXCEL_FUNCTION(185)
DECLARE_EXCEL_FUNCTION(186)
DECLARE_EXCEL_FUNCTION(187)
DECLARE_EXCEL_FUNCTION(188)
DECLARE_EXCEL_FUNCTION(189)
DECLARE_EXCEL_FUNCTION(190)
DECLARE_EXCEL_FUNCTION(191)
DECLARE_EXCEL_FUNCTION(192)
DECLARE_EXCEL_FUNCTION(193)
DECLARE_EXCEL_FUNCTION(194)
DECLARE_EXCEL_FUNCTION(195)
DECLARE_EXCEL_FUNCTION(196)
DECLARE_EXCEL_FUNCTION(197)
DECLARE_EXCEL_FUNCTION(198)
DECLARE_EXCEL_FUNCTION(199)
DECLARE_EXCEL_FUNCTION(200)

#ifdef __cplusplus
}
#endif