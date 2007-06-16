/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "WinRun4J.h"
#include "xlcall.h"
#include <vector>

using namespace std;

#define ADDIN_CLASS "addin.class"

// These are used to provide a diagnosis of any error. Each module has a function [ModuleName]_GetLastError().
static char g_modulename[MAX_PATH];
static bool g_error = false;
static char g_errorText[MAX_PATH];

// The addin/function implementations
static jobject g_addin = NULL;
static vector<jobject> g_functions;
static char g_addinName[MAX_PATH];
static char g_addinLongName[MAX_PATH];
static HINSTANCE g_hinstance;

// A helper function used to register a function
int RegisterFunction(LPXLOPER xllName, 
					  LPSTR procedure, LPSTR typeText, LPSTR functionText,
					  LPSTR argumentText, LPSTR macroType, LPSTR category,
					  LPSTR shortcutText, LPSTR helpTopic, 
					  LPSTR functionHelp, LPSTR argumentHelp)
{
	XLOPER result, args[10];
	result.xltype = xltypeStr;
	for(int i = 0; i < 10; i++) {
		args[i].xltype = xltypeStr;
	}
	args[0].val.str = procedure;
	args[1].val.str = typeText;
	args[2].val.str = functionText;
	args[3].val.str = argumentText;
	args[4].val.str = macroType;
	args[5].val.str = category;
	args[6].val.str = shortcutText;
	args[7].val.str = helpTopic;
	args[8].val.str = functionHelp;
	args[9].val.str = argumentHelp;
	int res = Excel4(xlfRegister, &result, 11, (LPXLOPER) &xllName, (LPXLOPER) &args[0], 
		(LPXLOPER) &args[1], (LPXLOPER) &args[2], (LPXLOPER) &args[3],
		(LPXLOPER) &args[4], (LPXLOPER) &args[5], (LPXLOPER) &args[6], 
		(LPXLOPER) &args[7], (LPXLOPER) &args[8], (LPXLOPER) &args[9]);

	if(res != 0) {
		g_error = true;
		if(result.xltype == xltypeStr)
			sprintf_s(g_errorText, MAX_PATH, "%s", result.val.str);
		else
			sprintf_s(g_errorText, MAX_PATH, "Could not register function");
	}

	return res;
}

bool StartupVM()
{
	// Attempt to load the ini file for this module
	dictionary* ini = WinRun4J::LoadIniFile(g_hinstance);

	if(ini == NULL) {
		g_error = true;
		sprintf_s(g_errorText, MAX_PATH, "Could not find module ini file");
		return false;
	}

	// Attempt to find an appropriate java VM
	char* vmlibrary = VM::FindJavaVMLibrary(ini);
	if(!vmlibrary) {
		g_error = true;
		sprintf_s(g_errorText, MAX_PATH, "Could not find VM");
		return false;
	}

	// Collect the VM args from the INI file
	TCHAR *vmargs[MAX_PATH];
	int vmargsCount = 0;
	WinRun4J::GetNumberedKeysFromIni(ini, VM_ARG, vmargs, vmargsCount);

	// Build up the classpath and add to vm args
	Classpath::BuildClassPath(ini, vmargs, vmargsCount);

	// Extract the specific VM args
	VM::ExtractSpecificVMArgs(ini, vmargs, vmargsCount);

	// Make sure there is a NULL at the end of the args
	vmargs[vmargsCount] = NULL;

	// Fire up the VM
	int vmerr = JNI::StartJavaVM(vmlibrary, vmargs);
	if(vmerr != 0) {
		g_error = true;
		sprintf_s(g_errorText, MAX_PATH, "VM could not be started (returned %d)", vmerr);
		return false;
	}

	// Grab reference to Java environment
	JNIEnv* env = JNI::GetJNIEnv();

	// Extract addin class name and convert to correct format
	char* addinClassName = iniparser_getstr(ini, ADDIN_CLASS);
	if(addinClassName == NULL) {
		g_error = true;
		sprintf_s(g_errorText, MAX_PATH, "addin class not specified");
		return false;
	}
	int len = strlen(addinClassName);
	for(int i = 0; i < len; i++) {
		if(addinClassName[i] == '.')
			addinClassName[i] = '/';
	}

	// Create new instance of addin
	jclass addinClass = env->FindClass(addinClassName);
	if(addinClass == NULL) {
		g_error = true;
		sprintf_s(g_errorText, MAX_PATH, "addin class could not be found by VM");
		return false;
	}

	jmethodID ctor = env->GetMethodID(addinClass, "<init>",  "()V");
	if(ctor == NULL) {
		g_error = true;
		sprintf_s(g_errorText, MAX_PATH, "could not find default constructor on addin class");
		return false;
	}

	g_addin = env->NewObject(addinClass, ctor);
	if(g_addin == NULL) {
		g_error = true;
		sprintf_s(g_errorText, MAX_PATH, "could not create addin class");
		return false;
	}

	// Free vm args
	for(int i = 0; i < vmargsCount; i++) {
		free(vmargs[i]);
	}

	// Free ini file
	iniparser_freedict(ini);

	// OK
	return true;
}

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
		int len = strlen(mname);
		mname[len - 4] = 0;
		for(int i = len - 4; i >= 0; i--) {
			if(mname[i] == '\\' || mname[i] == '/') {
				strcpy_s(g_modulename, MAX_PATH, &mname[i+1]);
				break;
			}
		}
	}

	if(fdwReason = DLL_PROCESS_DETACH)
	{
		// Destroy VM (note that this may block if there are non-daemon threads created in the VM)
		JNI::CleanupVM();
	}

	return TRUE;
}

// Convert from XLOPER struct to Oper class
jobject Convert(LPXLOPER px)
{
	return NULL;
}

// Convert form Oper class to XLOPER struct
void Convert(jobject oper, LPXLOPER px)
{
}

#ifdef __cplusplus
extern "C" {  
#endif 

__declspec(dllexport) int __cdecl xlAutoOpen(void)
{
	static XLOPER xDLL;
	Excel4(xlGetName, &xDLL, 0);

	// TODO register functions
	bool vmRes = StartupVM();

	// Register function for retrieving error text
	char helpFunction[MAX_PATH];
	sprintf_s(helpFunction, MAX_PATH, "%s_GetAddinError", g_modulename);
	int res = RegisterFunction(&xDLL, "GetAddinError", "S", helpFunction, "", "", g_modulename, "", "", "", "");

	// Free the XLL filename
	Excel4(xlFree, 0, 1, (LPXLOPER) &xDLL);

	return 1;
}

__declspec(dllexport) int __cdecl xlAutoClose(void)
{
	return 1;
}

__declspec(dllexport) LPXLOPER __cdecl xlAutoRegister(LPXLOPER pxName)
{
	static XLOPER xDLL, xRegId;

	xRegId.xltype = xltypeErr;
	xRegId.val.err = xlerrValue;

	// TODO register functions

	return (LPXLOPER) &xRegId;
}

__declspec(dllexport) int __cdecl xlAutoAdd(void)
{
	return 1;
}

__declspec(dllexport) int __cdecl xlAutoRemove(void)
{
	return 1;
}

__declspec(dllexport) LPXLOPER __cdecl xlAddInManagerInfo(LPXLOPER xAction)
{
	return NULL;
}

__declspec(dllexport) LPXLOPER __cdecl GetAddinError(LPXLOPER xAction)
{
	static XLOPER err;
	err.xltype = xltypeStr;
	err.val.str = g_error ? g_errorText : "";
	return &err;
}

void InvokeFunction(int number, LPXLOPER result, LPXLOPER px)
{
}

#define DECLARE_EXCEL_FUNCTION(number) \
__declspec(dllexport) LPXLOPER __cdecl EA4JFunc##number (LPXLOPER px) \
{ \
static XLOPER result;	\
InvokeFunction(number, &result, px); \
return &result; \
}

DECLARE_EXCEL_FUNCTION(1)
DECLARE_EXCEL_FUNCTION(2)
DECLARE_EXCEL_FUNCTION(3)
DECLARE_EXCEL_FUNCTION(4)
DECLARE_EXCEL_FUNCTION(5)



#ifdef __cplusplus
}
#endif