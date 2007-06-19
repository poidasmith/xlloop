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
#include <varargs.h>

using namespace std;

#define ADDIN_CLASS ":addin.class"
#define MAX_ARGS 30

// These are used to provide a diagnosis of any error. Each module has a function [ModuleName]_GetLastError().
static char g_modulename[MAX_PATH];
static bool g_error = false;
static char g_errorText[MAX_PATH];

// The addin/function implementations
static vector<jobject> g_functions;
static char g_addinName[MAX_PATH];
static char g_addinLongName[MAX_PATH];
static HINSTANCE g_hinstance;

// Cache of method/class/field references for performance
static jclass g_functionClass;
static jclass g_operClass;
static jmethodID g_operConstructor;
static jfieldID g_operTypeField;
static jfieldID g_operObjectValField;
static jfieldID g_operDoubleValField;
static jfieldID g_operIntegerValField;
static jfieldID g_operBooleanValField;
static jmethodID g_executeMethod;

// Clear JNI exception
void ClearJavaException(JNIEnv* env)
{
	if(env->ExceptionCheck()) {
		env->ExceptionClear();
	}
}

// Set the error
void SetLastError(const char* format, ...)
{
	ClearJavaException(JNI::GetJNIEnv());
	g_error = true;
	va_list args;
	va_start(args, format);
	vsprintf_s(g_errorText, MAX_PATH, format, args);
	fflush(stdout);
	fflush(stderr);
	va_end(args);
}

// Make an excel string
LPSTR MakeExcelString(const char* string)
{
	if(string == NULL) return NULL;
	size_t len = strlen(string);
	char* temp = (char *) malloc(len + 2);
	sprintf_s(temp, len + 2, " %s", string);
	temp[0] = (BYTE) len;
	return temp;
}

const char* CallJavaStringMethod(JNIEnv* env, jclass clazz, jobject obj, char* name)
{
	jmethodID methodID = env->GetMethodID(clazz, name, "()Ljava/lang/String;");
	if(methodID == NULL) {
		SetLastError("Could not find '%s' method", name);
		return NULL;
	}
	jstring str = (jstring) env->CallObjectMethod(obj, methodID);
	if(str == NULL) {
		return NULL;
	}
	jboolean iscopy = false;
	return env->GetStringUTFChars(str, &iscopy);
}

// A helper function used to register a function
int RegisterFunction(LPXLOPER xllName, 
					  const char* procedure, const char* typeText, const char* functionText,
					  const char* argumentText, const char* macroType, const char* category,
					  const char* shortcutText, const char* helpTopic, 
					  const char* functionHelp, const char* argumentHelp)
{
	XLOPER args[10];
	for(int i = 0; i < 10; i++) {
		args[i].xltype = xltypeStr;
	}
	args[0].val.str = MakeExcelString(procedure);
	args[1].val.str = MakeExcelString(typeText);
	args[2].val.str = MakeExcelString(functionText);
	args[3].val.str = MakeExcelString(argumentText);
	args[4].val.str = MakeExcelString(macroType);
	args[5].val.str = MakeExcelString(category);
	args[6].val.str = MakeExcelString(shortcutText);
	args[7].val.str = MakeExcelString(helpTopic);
	args[8].val.str = MakeExcelString(functionHelp);
	args[9].val.str = MakeExcelString(argumentHelp);

	// Check types for NULL
	for(int i = 0; i < 10; i++) {
		if(args[i].val.str == NULL)
			args[i].xltype = xltypeMissing;
	}

	int res = Excel4(xlfRegister, 0, 11, (LPXLOPER) xllName, (LPXLOPER) &args[0], 
		(LPXLOPER) &args[1], (LPXLOPER) &args[2], (LPXLOPER) &args[3],
		(LPXLOPER) &args[4], (LPXLOPER) &args[5], (LPXLOPER) &args[6], 
		(LPXLOPER) &args[7], (LPXLOPER) &args[8], (LPXLOPER) &args[9]);

	if(res != 0) {
		g_error = true;
		sprintf_s(g_errorText, MAX_PATH, "Could not register function: %s", functionText);
	}

	// Free strings
	for(int i = 0; i < 10; i++) {
		if(!args[i].val.str == NULL)
			free(args[i].val.str);
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
		SetLastError("VM could not be started (returned %d)", vmerr);
		return false;
	}

	// Grab reference to Java environment
	JNIEnv* env = JNI::GetJNIEnv();

	// Extract addin class name and convert to correct format
	char* addinClassName = iniparser_getstr(ini, ADDIN_CLASS);
	if(addinClassName == NULL) {
		SetLastError("addin class not specified");
		return false;
	}
	size_t len = strlen(addinClassName);
	for(size_t i = 0; i < len; i++) {
		if(addinClassName[i] == '.')
			addinClassName[i] = '/';
	}

	// Create new instance of addin
	jclass addinClass = env->FindClass(addinClassName);
	if(addinClass == NULL) {
		SetLastError("addin class could not be found by VM");
		return false;
	}

	jmethodID ctor = env->GetMethodID(addinClass, "<init>",  "()V");
	if(ctor == NULL) {
		SetLastError("could not find default constructor on addin class");
		return false;
	}

	jobject g_addin = env->NewObject(addinClass, ctor);
	if(g_addin == NULL) {
		SetLastError("could not create addin class");
		return false;
	}

	// Call the getName method
	const char* addinName = CallJavaStringMethod(env, addinClass, g_addin, "getName");
	if(addinName == NULL) {
		return false;
	}
	strcpy_s(g_addinName, MAX_PATH, addinName);

	// Call the get long name method
	const char* addinLongName = CallJavaStringMethod(env, addinClass, g_addin, "getLongName");
	if(addinLongName == NULL) {
		return false;
	}
	strcpy_s(g_addinLongName, MAX_PATH, addinLongName);

	// Call the getFunctions method
	jmethodID fmeth = env->GetMethodID(addinClass, "getFunctions", "()[Lorg/boris/excel4j/Function;");
	if(fmeth == NULL) {
		SetLastError("could not find getFunctions method");
		return false;
	}
	jobjectArray functions = (jobjectArray) env->CallObjectMethod(g_addin, fmeth);

	// Add functions to our lookup table
	if(functions != NULL) {
		jsize flen = env->GetArrayLength(functions);
		for(int i = 0; i < flen; i++) {
			jobject obj = env->GetObjectArrayElement(functions, i);
			obj = env->NewGlobalRef(obj); // This is so the garbage collector doesn't destroy it
			g_functions.push_back(obj);
		}
	}

	// Store a reference to the Function class
	g_functionClass = env->FindClass("org/boris/excel4j/Function");
	if(g_functionClass == NULL) {
		SetLastError("Could not find Function class");
		return false;
	}
	
	// Store a reference to the Function.execute method
	g_executeMethod = env->GetMethodID(g_functionClass, "execute", "([Lorg/boris/excel4j/Oper;)Lorg/boris/excel4j/Oper;");
	if(g_executeMethod == NULL) {
		SetLastError("Could not find Function.execute method");
		return false;
	}

	// Store a reference to the Oper class
	g_operClass = env->FindClass("org/boris/excel4j/Oper");
	if(g_operClass == NULL) {
		SetLastError("Could not find Oper class");
		return false;
	}

	// Store reference to the Oper constructor
	g_operConstructor = env->GetMethodID(g_operClass, "<init>",  "()V");
	if(g_operConstructor == NULL) {
		SetLastError("Could not find default constructor for Oper class");
		return false;
	}

	// Store reference to the Oper.xlType field
	g_operTypeField = env->GetFieldID(g_operClass, "xlType", "I");
	if(g_operTypeField == NULL) {
		SetLastError("Could not find Oper.xlType field");
		return false;
	}

	// Store reference to the Oper.objectValue field
	g_operObjectValField = env->GetFieldID(g_operClass, "objectValue", "Ljava/lang/Object;");
	if(g_operObjectValField == NULL) {
		SetLastError("Could not find Oper.objectValue field");
		return false;
	}

	// Store reference to the Oper.doubleValue field
	g_operDoubleValField = env->GetFieldID(g_operClass, "doubleValue", "D");
	if(g_operDoubleValField == NULL) {
		SetLastError("Could not find Oper.doubleValue field");
		return false;
	}

	// Store reference to the Oper.integerValue field
	g_operIntegerValField = env->GetFieldID(g_operClass, "integerValue", "I");
	if(g_operIntegerValField == NULL) {
		SetLastError("Could not find Oper.integerValue field");
		return false;
	}

	// Store reference to the Oper.booleanValue field
	g_operBooleanValField = env->GetFieldID(g_operClass, "booleanValue", "Z");
	if(g_operBooleanValField == NULL) {
		SetLastError("Could not find Oper.booleanValue field");
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

// Convert from XLOPER struct to Oper class
void Convert(JNIEnv* env, LPXLOPER px, jobject obj)
{
	//jobject obj = env->NewObject(g_operClass, g_operConstructor);
	env->SetIntField(obj, g_operTypeField, px->xltype);
	switch(px->xltype)
	{
	case xltypeBigData:
		break;
	case xltypeBool:
		env->SetBooleanField(obj, g_operBooleanValField, px->val.boolean);
		break;
	case xltypeErr:
		env->SetIntField(obj, g_operIntegerValField, px->val.err);
		break;
	case xltypeFlow:
		break;
	case xltypeInt:
		env->SetIntField(obj, g_operIntegerValField, px->val.w);
		break;
	case xltypeMissing:
		break;
	case xltypeMulti:
		break;
	case xltypeNil:
		break;
	case xltypeNum:
		env->SetDoubleField(obj, g_operDoubleValField, px->val.num);
		break;
	case xltypeRef:
		break;
	case xltypeSRef:
		break;
	case xltypeStr:
		{
			char chars[MAX_PATH];
			strncpy_s(chars, MAX_PATH, &px->val.str[1], px->val.str[0]);
			env->SetObjectField(obj, g_operObjectValField, env->NewStringUTF(chars));
		}
		break;
	}
}

// Convert form Oper class to XLOPER struct
void Convert(JNIEnv* env, jobject oper, LPXLOPER px)
{
	static jboolean iscopy = false;
	px->xltype = env->GetIntField(oper, g_operTypeField);
	switch(px->xltype)
	{
	case xltypeBigData:
		break;
	case xltypeBool:
		px->val.boolean = env->GetBooleanField(oper, g_operBooleanValField);
		break;
	case xltypeErr:
		px->val.err = env->GetIntField(oper, g_operIntegerValField);
		break;
	case xltypeFlow:
		break;
	case xltypeInt:
		px->val.w = env->GetIntField(oper, g_operIntegerValField);
		break;
	case xltypeMissing:
		break;
	case xltypeMulti:
		break;
	case xltypeNil:
		break;
	case xltypeNum:
		px->val.num = env->GetDoubleField(oper, g_operDoubleValField);
		break;
	case xltypeRef:
		break;
	case xltypeSRef:
		break;
	case xltypeStr:
		{
			px->val.str = MakeExcelString(env->GetStringUTFChars((jstring)env->GetObjectField(oper, g_operObjectValField), &iscopy));
		}
		break;
	}
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
	int res = RegisterFunction(&xDLL, "EA4JGetLastError", "R", errorFunction, 
		NULL, "1", "Information", NULL, NULL, NULL, NULL);

	// Fire up the VM
	bool vmRes = StartupVM();

	// Register java functions (if VM started ok)
	if(vmRes) {
		char javaFunction[MAX_PATH];
		JNIEnv* env = JNI::GetJNIEnv();
		for(size_t i = 0; i < g_functions.size(); i++) {
			jobject obj = g_functions[i];
			sprintf_s(javaFunction, MAX_PATH, "EA4JFunc%d", (i + 1));
			RegisterFunction(&xDLL, javaFunction, "RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR",
				CallJavaStringMethod(env, g_functionClass, obj, "getFunctionText"),
				CallJavaStringMethod(env, g_functionClass, obj, "getArgumentText"),
				CallJavaStringMethod(env, g_functionClass, obj, "getMacroType"),
				CallJavaStringMethod(env, g_functionClass, obj, "getCategory"),
				CallJavaStringMethod(env, g_functionClass, obj, "getShortcutText"),
				CallJavaStringMethod(env, g_functionClass, obj, "getHelpTopic"),
				CallJavaStringMethod(env, g_functionClass, obj, "getFunctionHelp"),
				CallJavaStringMethod(env, g_functionClass, obj, "getArgumentHelp"));
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
	JNI::CleanupVM();

	return 1;
}

__declspec(dllexport) LPXLOPER WINAPI xlAutoRegister(LPXLOPER pxName)
{
	static XLOPER xDLL, xRegId;

	xRegId.xltype = xltypeErr;
	xRegId.val.err = xlerrValue;

	// TODO register functions

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
	return NULL;
}

__declspec(dllexport) LPXLOPER WINAPI EA4JGetLastError(LPXLOPER args)
{
	static XLOPER err;
	static char errStr[MAX_PATH];
	sprintf_s(errStr, MAX_PATH, " %s", g_error ? g_errorText : "OK");
	errStr[0] = (BYTE) lstrlen(errStr+1);
	err.xltype = xltypeStr;
	err.val.str = errStr;
	return &err;
}

// Used to create an array to hold the args
jobjectArray CreateArgArray()
{
	JNIEnv* env = JNI::GetJNIEnv();
	jobjectArray arr = env->NewObjectArray(MAX_ARGS, g_operClass, NULL);
	for(int i = 0; i < MAX_ARGS; i++) {
		env->SetObjectArrayElement(arr, i, env->NewObject(g_operClass, g_operConstructor));
	}
	arr = (jobjectArray) env->NewGlobalRef(arr);
	return arr;
}

/*
#define DECLARE_EXCEL_FUNCTION(number) \
__declspec(dllexport) LPXLOPER WINAPI EA4JFunc##number (LPXLOPER px1, LPXLOPER px2, LPXLOPER px3 \
	,LPXLOPER px4, LPXLOPER px5, LPXLOPER px6, LPXLOPER px7, LPXLOPER px8, LPXLOPER px9 \
	,LPXLOPER px10, LPXLOPER px11, LPXLOPER px12, LPXLOPER px13, LPXLOPER px14, LPXLOPER px15 \
	,LPXLOPER px16, LPXLOPER px17, LPXLOPER px18, LPXLOPER px19, LPXLOPER px20, LPXLOPER px21 \
	,LPXLOPER px22, LPXLOPER px23, LPXLOPER px24, LPXLOPER px25, LPXLOPER px26, LPXLOPER px27\
	,LPXLOPER px28, LPXLOPER px29, LPXLOPER px30) \
{ \
static XLOPER result;	\
static jobjectArray args = CreateArgArray(); \
result.xltype = xltypeErr; \
JNIEnv* env = JNI::GetJNIEnv(); \
env->SetObjectArrayElement(args, 0, Convert(env, px1)); \
env->SetObjectArrayElement(args, 1, Convert(env, px2)); \
env->SetObjectArrayElement(args, 2, Convert(env, px3)); \
env->SetObjectArrayElement(args, 3, Convert(env, px4)); \
env->SetObjectArrayElement(args, 4, Convert(env, px5)); \
env->SetObjectArrayElement(args, 5, Convert(env, px6)); \
env->SetObjectArrayElement(args, 6, Convert(env, px7)); \
env->SetObjectArrayElement(args, 7, Convert(env, px8)); \
env->SetObjectArrayElement(args, 8, Convert(env, px9)); \
env->SetObjectArrayElement(args, 9, Convert(env, px10)); \
env->SetObjectArrayElement(args, 10, Convert(env, px11)); \
env->SetObjectArrayElement(args, 11, Convert(env, px12)); \
env->SetObjectArrayElement(args, 12, Convert(env, px13)); \
env->SetObjectArrayElement(args, 13, Convert(env, px14)); \
env->SetObjectArrayElement(args, 14, Convert(env, px15)); \
env->SetObjectArrayElement(args, 15, Convert(env, px16)); \
env->SetObjectArrayElement(args, 16, Convert(env, px17)); \
env->SetObjectArrayElement(args, 17, Convert(env, px18)); \
env->SetObjectArrayElement(args, 18, Convert(env, px19)); \
env->SetObjectArrayElement(args, 19, Convert(env, px20)); \
env->SetObjectArrayElement(args, 20, Convert(env, px21)); \
env->SetObjectArrayElement(args, 21, Convert(env, px22)); \
env->SetObjectArrayElement(args, 22, Convert(env, px23)); \
env->SetObjectArrayElement(args, 23, Convert(env, px24)); \
env->SetObjectArrayElement(args, 24, Convert(env, px25)); \
env->SetObjectArrayElement(args, 25, Convert(env, px26)); \
env->SetObjectArrayElement(args, 26, Convert(env, px27)); \
env->SetObjectArrayElement(args, 27, Convert(env, px28)); \
env->SetObjectArrayElement(args, 28, Convert(env, px29)); \
env->SetObjectArrayElement(args, 29, Convert(env, px30)); \
jobject res = env->CallObjectMethod(g_functions[number], g_executeMethod); \
Convert(env, res, &result); \
return &result; \
}
*/

__declspec(dllexport) LPXLOPER WINAPI EA4JFunc1 ( LPXLOPER px1, LPXLOPER px2, LPXLOPER px3 \
	,LPXLOPER px4, LPXLOPER px5, LPXLOPER px6, LPXLOPER px7, LPXLOPER px8, LPXLOPER px9 \
	,LPXLOPER px10, LPXLOPER px11, LPXLOPER px12, LPXLOPER px13, LPXLOPER px14, LPXLOPER px15 \
	,LPXLOPER px16, LPXLOPER px17, LPXLOPER px18, LPXLOPER px19, LPXLOPER px20, LPXLOPER px21 \
	,LPXLOPER px22, LPXLOPER px23, LPXLOPER px24, LPXLOPER px25, LPXLOPER px26, LPXLOPER px27\
	,LPXLOPER px28, LPXLOPER px29, LPXLOPER px30 )
{  
	static XLOPER result;
	static jobjectArray args = CreateArgArray();
	result.xltype = xltypeErr;
	JNIEnv* env = JNI::GetJNIEnv(); 
	int i = 0;
	Convert(env, px1, env->GetObjectArrayElement(args, i++));
	Convert(env, px2, env->GetObjectArrayElement(args, i++));
	Convert(env, px3, env->GetObjectArrayElement(args, i++));
	Convert(env, px4, env->GetObjectArrayElement(args, i++));
	Convert(env, px5, env->GetObjectArrayElement(args, i++));
	Convert(env, px6, env->GetObjectArrayElement(args, i++));
	Convert(env, px7, env->GetObjectArrayElement(args, i++));
	Convert(env, px8, env->GetObjectArrayElement(args, i++));
	Convert(env, px9, env->GetObjectArrayElement(args, i++));
	Convert(env, px10, env->GetObjectArrayElement(args, i++));
	Convert(env, px11, env->GetObjectArrayElement(args, i++));
	Convert(env, px12, env->GetObjectArrayElement(args, i++));
	Convert(env, px13, env->GetObjectArrayElement(args, i++));
	Convert(env, px14, env->GetObjectArrayElement(args, i++));
	Convert(env, px15, env->GetObjectArrayElement(args, i++));
	Convert(env, px16, env->GetObjectArrayElement(args, i++));
	Convert(env, px17, env->GetObjectArrayElement(args, i++));
	Convert(env, px18, env->GetObjectArrayElement(args, i++));
	Convert(env, px19, env->GetObjectArrayElement(args, i++));
	Convert(env, px20, env->GetObjectArrayElement(args, i++));
	Convert(env, px21, env->GetObjectArrayElement(args, i++));
	Convert(env, px22, env->GetObjectArrayElement(args, i++));
	Convert(env, px23, env->GetObjectArrayElement(args, i++));
	Convert(env, px24, env->GetObjectArrayElement(args, i++));
	Convert(env, px25, env->GetObjectArrayElement(args, i++));
	Convert(env, px26, env->GetObjectArrayElement(args, i++));
	Convert(env, px27, env->GetObjectArrayElement(args, i++));
	Convert(env, px28, env->GetObjectArrayElement(args, i++));
	Convert(env, px29, env->GetObjectArrayElement(args, i++));
	Convert(env, px30, env->GetObjectArrayElement(args, i++));

	jobject res = env->CallObjectMethod(g_functions[0], g_executeMethod, args); 
	ClearJavaException(env);
	Convert(env, res, &result); 

	return &result; 
}

#define DECLARE_EXCEL_FUNCTION(number) \
__declspec(dllexport) LPXLOPER WINAPI EA4JFunc##number (...) \
{ \
static XLOPER result;	\
return &result; \
}

//DECLARE_EXCEL_FUNCTION(1)
DECLARE_EXCEL_FUNCTION(2)
DECLARE_EXCEL_FUNCTION(3)
DECLARE_EXCEL_FUNCTION(4)
DECLARE_EXCEL_FUNCTION(5)
DECLARE_EXCEL_FUNCTION(6)
DECLARE_EXCEL_FUNCTION(7)
DECLARE_EXCEL_FUNCTION(8)
DECLARE_EXCEL_FUNCTION(9)
DECLARE_EXCEL_FUNCTION(10)

#ifdef __cplusplus
}
#endif