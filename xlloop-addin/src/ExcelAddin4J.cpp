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
#include "xlcall.h"
#include <vector>
#include <varargs.h>
#include <jni.h>

using namespace std;

#define ADDIN_CLASS ":addin.class"
#define MAX_ARGS 30

// These are used to provide a diagnosis of any error. Each module has a function [ModuleName]_GetLastError().
static char g_modulename[MAX_PATH];

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
static jclass g_refClass;
static jmethodID g_refConstructor;
static jfieldID g_refRowFirstField;
static jfieldID g_refRowLastField;
static jfieldID g_refColFirstField;
static jfieldID g_refColLastField;

// Native methods
int JNICALL Excel4J(JNIEnv* env, jobject self, int xlfn, jobject result, jobjectArray args);
int JNICALL XLCallVerJ(JNIEnv* env, jobject self);

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

// A helper function used to register a function
int RegisterFunction(LPXLOPER xllName, 
					  const char* procedure, const char* typeText, const char* functionText,
					  const char* argumentText, const char* macroType, const char* category,
					  const char* shortcutText, const char* helpTopic, 
					  const char* functionHelp, const char* argumentHelp)
{
	XLOPER args[10];
	for(int i = 0; i < 10; i++) {
		args[i].xltype = xltypeStr | xlbitXLFree;
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
		Log::SetLastError("Could not register function: %s", functionText);
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
	dictionary* ini = INI::LoadIniFile(g_hinstance);

	if(ini == NULL) {
		Log::SetLastError("Could not find module ini file");
		return false;
	}

	// Attempt to find an appropriate java VM
	char* vmlibrary = VM::FindJavaVMLibrary(ini);
	if(!vmlibrary) {
		Log::SetLastError("Could not find VM");
		return false;
	}

	// Collect the VM args from the INI file
	TCHAR *vmargs[MAX_PATH];
	int vmargsCount = 0;
	INI::GetNumberedKeysFromIni(ini, VM_ARG, vmargs, vmargsCount);

	// Build up the classpath and add to vm args
	Classpath::BuildClassPath(ini, vmargs, vmargsCount);

	// Extract the specific VM args
	VM::ExtractSpecificVMArgs(ini, vmargs, vmargsCount);

	// Make sure there is a NULL at the end of the args
	vmargs[vmargsCount] = NULL;

	// Fire up the VM
	int vmerr = VM::StartJavaVM(vmlibrary, vmargs);
	if(vmerr != 0) {
		Log::SetLastError("VM could not be started (returned %d)", vmerr);
		return false;
	}

	// Grab reference to Java environment
	JNIEnv* env = VM::GetJNIEnv();

	// Extract addin class name and convert to correct format
	char* addinClassName = iniparser_getstr(ini, ADDIN_CLASS);
	if(addinClassName == NULL) {
		Log::SetLastError("addin class not specified");
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
		Log::SetLastError("addin class could not be found by VM");
		return false;
	}

	jmethodID ctor = env->GetMethodID(addinClass, "<init>",  "()V");
	if(ctor == NULL) {
		Log::SetLastError("could not find default constructor on addin class");
		return false;
	}

	jobject g_addin = env->NewObject(addinClass, ctor);
	if(g_addin == NULL) {
		Log::SetLastError("could not create addin class");
		return false;
	}

	// Call the getName method
	const char* addinName = JNI::CallJavaStringMethod(env, addinClass, g_addin, "getName");
	if(addinName == NULL) {
		return false;
	}
	strcpy_s(g_addinName, MAX_PATH, addinName);

	// Call the get long name method
	const char* addinLongName = JNI::CallJavaStringMethod(env, addinClass, g_addin, "getLongName");
	if(addinLongName == NULL) {
		return false;
	}
	strcpy_s(g_addinLongName, MAX_PATH, addinLongName);

	// Call the getFunctions method
	jmethodID fmeth = env->GetMethodID(addinClass, "getFunctions", "()[Lorg/excel4j/Function;");
	if(fmeth == NULL) {
		Log::SetLastError("could not find getFunctions method");
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
	g_functionClass = env->FindClass("org/excel4j/Function");
	if(g_functionClass == NULL) {
		Log::SetLastError("Could not find Function class");
		return false;
	}
	
	// Store a reference to the Function.execute method
	g_executeMethod = env->GetMethodID(g_functionClass, "execute", "([Lorg/excel4j/Oper;)Lorg/excel4j/Oper;");
	if(g_executeMethod == NULL) {
		Log::SetLastError("Could not find Function.execute method");
		return false;
	}

	// Store a reference to the Oper class
	g_operClass = env->FindClass("org/excel4j/Oper");
	if(g_operClass == NULL) {
		Log::SetLastError("Could not find Oper class");
		return false;
	}

	// Store reference to the Oper constructor
	g_operConstructor = env->GetMethodID(g_operClass, "<init>",  "()V");
	if(g_operConstructor == NULL) {
		Log::SetLastError("Could not find default constructor for Oper class");
		return false;
	}

	// Store reference to the Oper.xlType field
	g_operTypeField = env->GetFieldID(g_operClass, "xlType", "I");
	if(g_operTypeField == NULL) {
		Log::SetLastError("Could not find Oper.xlType field");
		return false;
	}

	// Store reference to the Oper.objectValue field
	g_operObjectValField = env->GetFieldID(g_operClass, "objectValue", "Ljava/lang/Object;");
	if(g_operObjectValField == NULL) {
		Log::SetLastError("Could not find Oper.objectValue field");
		return false;
	}

	// Store reference to the Oper.doubleValue field
	g_operDoubleValField = env->GetFieldID(g_operClass, "doubleValue", "D");
	if(g_operDoubleValField == NULL) {
		Log::SetLastError("Could not find Oper.doubleValue field");
		return false;
	}

	// Store reference to the Oper.integerValue field
	g_operIntegerValField = env->GetFieldID(g_operClass, "integerValue", "I");
	if(g_operIntegerValField == NULL) {
		Log::SetLastError("Could not find Oper.integerValue field");
		return false;
	}

	// Store reference to the Oper.booleanValue field
	g_operBooleanValField = env->GetFieldID(g_operClass, "booleanValue", "Z");
	if(g_operBooleanValField == NULL) {
		Log::SetLastError("Could not find Oper.booleanValue field");
		return false;
	}

	jclass excelClass = env->FindClass("org/excel4j/Excel");
	if(excelClass == NULL) {
		Log::SetLastError("Could not find Excel class");
		return false;
	}

	// Register excel native function
	JNINativeMethod nm[2];
	nm[0].name = "Excel4";
	nm[0].signature = "(ILorg/excel4j/Oper;[Lorg/excel4j/Oper;)I";
	nm[0].fnPtr = Excel4J;
	nm[1].name = "XLCallVer";
	nm[1].signature = "()I";
	nm[1].fnPtr = XLCallVerJ;
	env->RegisterNatives(excelClass, nm, 2);

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
	env->SetIntField(obj, g_operTypeField, px->xltype);
	env->SetObjectField(obj,  g_operObjectValField, NULL);
	switch(px->xltype & ~(xlbitXLFree | xlbitDLLFree))
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
		{
			jclass refClass = env->FindClass("org/excel4j/Reference");
			jmethodID cons = env->GetMethodID(refClass, "<init>", "()V");
			jobject ref = env->NewObject(refClass, cons);
			jfieldID rwf = env->GetFieldID(refClass, "rwFirst", "I");
			jfieldID rwl = env->GetFieldID(refClass, "rwLast", "I");
			jfieldID clf = env->GetFieldID(refClass, "colFirst", "I");
			jfieldID cll = env->GetFieldID(refClass, "colLast", "I");
			env->SetIntField(ref, rwf, px->val.sref.ref.rwFirst);
			env->SetIntField(ref, rwl, px->val.sref.ref.rwLast);
			env->SetIntField(ref, clf, px->val.sref.ref.colFirst);
			env->SetIntField(ref, cll, px->val.sref.ref.colLast);
			env->SetObjectField(obj, g_operObjectValField, ref);
			env->SetIntField(obj, g_operIntegerValField, px->val.mref.idSheet);
		}
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
	if(oper == NULL) return;

	static jboolean iscopy = false;
	px->xltype = env->GetIntField(oper, g_operTypeField);
	switch(px->xltype & ~(xlbitXLFree | xlbitDLLFree))
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
		{
			jclass refClass = env->FindClass("org/excel4j/Reference");
			jobject ref = env->GetObjectField(oper, g_operObjectValField);
			jfieldID rwf = env->GetFieldID(refClass, "rwFirst", "I");
			jfieldID rwl = env->GetFieldID(refClass, "rwLast", "I");
			jfieldID clf = env->GetFieldID(refClass, "colFirst", "I");
			jfieldID cll = env->GetFieldID(refClass, "colLast", "I");
			px->val.sref.count = 1;
			px->val.sref.ref.rwFirst = env->GetIntField(ref, rwf);
			px->val.sref.ref.rwLast = env->GetIntField(ref, rwl);
			px->val.sref.ref.colFirst = env->GetIntField(ref, clf);
			px->val.sref.ref.colLast = env->GetIntField(ref, cll);
			px->val.mref.idSheet = env->GetIntField(oper, g_operIntegerValField);
		}
		break;
	case xltypeSRef:
		break;
	case xltypeStr:
		px->val.str = MakeExcelString(env->GetStringUTFChars((jstring)env->GetObjectField(oper, g_operObjectValField), &iscopy));
		break;
	}
}

int JNICALL Excel4J(JNIEnv* env, jobject self, int xlfn, jobject result, jobjectArray args)
{
	static XLOPER res;
	int numArgs = env->GetArrayLength(args);
	res.xltype = xltypeMissing;
	XLOPER *operArgs = new XLOPER[numArgs];
	for(int i = 0; i < numArgs; i++) {
		Convert(env, env->GetObjectArrayElement(args, i), &operArgs[i]);
	}
	int fRes = Excel4v(xlfn, (LPXLOPER) &res, numArgs, (LPXLOPER*) operArgs);
	Convert(env, &res, result);
	delete [] operArgs;
	return fRes;
}

int JNICALL XLCallVerJ(JNIEnv* env, jobject self)
{
	int val = XLCallVer();
	return val;
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
		JNIEnv* env = VM::GetJNIEnv();
		for(size_t i = 0; i < g_functions.size(); i++) {
			jobject obj = g_functions[i];
			sprintf_s(javaFunction, MAX_PATH, "EA4JFunc%d", (i + 1));
			RegisterFunction(&xDLL, javaFunction, "RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR",
				JNI::CallJavaStringMethod(env, g_functionClass, obj, "getFunctionText"),
				JNI::CallJavaStringMethod(env, g_functionClass, obj, "getArgumentText"),
				JNI::CallJavaStringMethod(env, g_functionClass, obj, "getMacroType"),
				JNI::CallJavaStringMethod(env, g_functionClass, obj, "getCategory"),
				JNI::CallJavaStringMethod(env, g_functionClass, obj, "getShortcutText"),
				JNI::CallJavaStringMethod(env, g_functionClass, obj, "getHelpTopic"),
				JNI::CallJavaStringMethod(env, g_functionClass, obj, "getFunctionHelp"),
				JNI::CallJavaStringMethod(env, g_functionClass, obj, "getArgumentHelp"));
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
	const char* error = Log::GetLastError();
	sprintf_s(errStr, MAX_PATH, " %s", error != NULL ? error : "OK");
	errStr[0] = (BYTE) lstrlen(errStr+1);
	err.xltype = xltypeStr;
	err.val.str = errStr;
	return &err;
}

// Used to create an array to hold the args
jobjectArray CreateArgArray()
{
	JNIEnv* env = VM::GetJNIEnv();
	jobjectArray arr = env->NewObjectArray(MAX_ARGS, g_operClass, NULL);
	for(int i = 0; i < MAX_ARGS; i++) {
		env->SetObjectArrayElement(arr, i, env->NewObject(g_operClass, g_operConstructor));
	}
	arr = (jobjectArray) env->NewGlobalRef(arr);
	return arr;
}

#define DECLARE_EXCEL_FUNCTION(number) \
__declspec(dllexport) LPXLOPER WINAPI EA4JFunc##number (LPXLOPER px1, LPXLOPER px2, LPXLOPER px3 \
	,LPXLOPER px4, LPXLOPER px5, LPXLOPER px6, LPXLOPER px7, LPXLOPER px8, LPXLOPER px9 \
	,LPXLOPER px10, LPXLOPER px11, LPXLOPER px12, LPXLOPER px13, LPXLOPER px14, LPXLOPER px15 \
	,LPXLOPER px16, LPXLOPER px17, LPXLOPER px18, LPXLOPER px19, LPXLOPER px20, LPXLOPER px21 \
	,LPXLOPER px22, LPXLOPER px23, LPXLOPER px24, LPXLOPER px25, LPXLOPER px26, LPXLOPER px27\
	,LPXLOPER px28, LPXLOPER px29, LPXLOPER px30) \
{ \
	static XLOPER result; \
	static jobjectArray args = CreateArgArray(); \
	result.xltype = xltypeErr; \
	JNIEnv* env = VM::GetJNIEnv(); \
	int i = 0; \
	Convert(env, px1, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px2, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px3, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px4, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px5, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px6, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px7, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px8, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px9, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px10, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px11, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px12, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px13, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px14, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px15, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px16, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px17, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px18, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px19, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px20, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px21, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px22, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px23, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px24, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px25, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px26, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px27, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px28, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px29, env->GetObjectArrayElement(args, i++)); \
	Convert(env, px30, env->GetObjectArrayElement(args, i++)); \
\
	jobject res = env->CallObjectMethod(g_functions[number - 1], g_executeMethod, args); \
	JNI::ClearJavaException(env); \
	Convert(env, res, &result); \
\
	return &result; \
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