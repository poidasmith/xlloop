/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "JXLL.h"
#include <jni.h>
#include "xlcall.h"

#define MAX_XLL_ARGS 30

static JavaVM* jvm;

// Cache fields for XLL class
static jclass XLL_CLASS;
static jmethodID XLL_XLCALLVER_METHOD;
static jmethodID XLL_EXCEL4_METHOD;

// Cache fields for Addin class
static jclass ADDIN_CLASS;
static jfieldID ADDIN_LIBRARY_FIELD;

// Cache fields for XLOper class
static jclass XLOPER_CLASS;
static jmethodID XLOPER_CONSTRUCTOR;
static jfieldID XLOPER_TYPE_FIELD;
static jfieldID XLOPER_NUM_FIELD;
static jfieldID XLOPER_STR_FIELD;
static jfieldID XLOPER_BOOL_FIELD;
static jfieldID XLOPER_ERR_FIELD;
static jfieldID XLOPER_W_FIELD;
static jfieldID XLOPER_ROWS_FIELD;
static jfieldID XLOPER_COLS_FIELD;
static jfieldID XLOPER_ARRAY_FIELD;
static jfieldID XLOPER_REF_FIELD;
static jfieldID XLOPER_MREF_FIELD;
static jfieldID XLOPER_IDSHEET_FIELD;

// Cache fields for XLOperHolder class
static jclass XLOPERHOLDER_CLASS;
static jmethodID XLOPERHOLDER_CONSTRUCTOR;
static jfieldID XLOPERHOLDER_VALUE_FIELD;

// Cache fields for XLRef class
static jclass XLREF_CLASS;
static jmethodID XLREF_CONSTRUCTOR;
static jfieldID XLREF_RWFIRST_FIELD;
static jfieldID XLREF_RWLAST_FIELD;
static jfieldID XLREF_COLFIRST_FIELD;
static jfieldID XLREF_COLLAST_FIELD;

// Arg types for calling methods
const int argVoid = 0;
const int argBoolean = 1;
const int argBooleanPtr = 2;
const int argDouble = 3;
const int argDoublePtr = 4;
const int argString = 5;
const int argStringLC = 6;
const int argUString = 7;
const int argUStringLC = 8;
const int argUShort = 9;
const int argShort = 10;
const int argShortPtr = 11;
const int argInt = 12;
const int argIntPtr = 13;
const int argArray = 14;
const int argFPArray = 15;
const int argXLOper = 16;
const int argXLOperRef = 17;

jobject Convert(JNIEnv* env, LPXLOPER oper);

LPSTR MakeExcelString(const char* string)
{
	if(string == NULL) return NULL;
	size_t len = strlen(string);
	if(len > 255) len = 255; // Excel strings are limited tvio 255 chars
	char* temp = (char *) malloc(len + 2);
	if(len) memcpy(temp + 1, string, len);
	temp[0] = (BYTE) len;
	return temp;
}

void Convert(JNIEnv* env, LPXLOPER oper, jobject o)
{
	char chars[MAX_PATH + 4];

	if(!oper) {
		env->SetIntField(o,  XLOPER_TYPE_FIELD, xltypeNil);
		return;
	}
	env->SetIntField(o,  XLOPER_TYPE_FIELD, oper->xltype & ~(xlbitXLFree | xlbitDLLFree));
	switch(oper->xltype & ~(xlbitXLFree | xlbitDLLFree)) {
		case xltypeNum:
			env->SetDoubleField(o, XLOPER_NUM_FIELD, oper->val.num);
			break;
		case xltypeErr:
			env->SetIntField(o, XLOPER_ERR_FIELD, oper->val.err);
			break;
		case xltypeInt:
			env->SetIntField(o, XLOPER_W_FIELD, oper->val.w);
			break;
		case xltypeBool:
			env->SetBooleanField(o, XLOPER_BOOL_FIELD, oper->val.boolean == 0 ? false : true);
			break;
		case xltypeStr:
			if(oper->val.str) {
				DWORD len = (oper->val.str[0] & 0xff);
				DWORD old = len;
				if(len > 254 || len < 0) {
					len = 254;
				}
				memcpy(&chars[4], &(oper->val.str[1]), len);
				chars[len+4] = 0;
				env->SetObjectField(o, XLOPER_STR_FIELD, env->NewStringUTF(&chars[4]));
			}
			break;
		case xltypeRef:
			{
				jobject ro = env->NewObject(XLREF_CLASS, XLREF_CONSTRUCTOR);
				env->SetIntField(o, XLREF_RWFIRST_FIELD, oper->val.sref.ref.rwFirst);
				env->SetIntField(o, XLREF_RWLAST_FIELD, oper->val.sref.ref.rwLast);
				env->SetIntField(o, XLREF_COLFIRST_FIELD, oper->val.sref.ref.colFirst);
				env->SetIntField(o, XLREF_COLLAST_FIELD, oper->val.sref.ref.colLast);
				env->SetObjectField(o, XLOPER_REF_FIELD, ro);
			}
			break;
		case xltypeMulti:
			{
				env->SetIntField(o, XLOPER_ROWS_FIELD, oper->val.array.rows);
				env->SetIntField(o, XLOPER_COLS_FIELD, oper->val.array.columns);
				int size = oper->val.array.rows * oper->val.array.columns;
				jobjectArray a = env->NewObjectArray(size, XLOPER_CLASS, NULL); 
				for(int i = 0; i < size; i++) {
					env->SetObjectArrayElement(a, i, Convert(env, &oper->val.array.lparray[i]));
				}
				env->SetObjectField(o, XLOPER_ARRAY_FIELD, a);
			}
			break;
	}
}

jobject Convert(JNIEnv* env, LPXLOPER oper)
{
	jobject o = env->NewObject(XLOPER_CLASS, XLOPER_CONSTRUCTOR);
	Convert(env, oper, o);
	return o;
}

void Convert(JNIEnv* env, jobject o, LPXLOPER oper) 
{
	if(o == NULL) {
		if(oper) oper->xltype = xltypeNil;
		return;
	}
	oper->xltype = (WORD) env->GetIntField(o, XLOPER_TYPE_FIELD);
	switch(oper->xltype) {
		case xltypeNum:
			oper->val.num = env->GetDoubleField(o, XLOPER_NUM_FIELD);
			break;
		case xltypeInt:
			oper->val.w = (WORD) env->GetIntField(o, XLOPER_W_FIELD);
			break;
		case xltypeErr:
			oper->val.err = (WORD) env->GetIntField(o, XLOPER_ERR_FIELD);
			break;
		case xltypeBool:
			oper->val.boolean = env->GetBooleanField(o, XLOPER_BOOL_FIELD);
			break;
		case xltypeStr: 
			{
				jstring s = (jstring) env->GetObjectField(o, XLOPER_STR_FIELD);
				jboolean iscopy = false;
				const char* cs = env->GetStringUTFChars(s, &iscopy);
				oper->val.str = MakeExcelString(cs);
				env->ReleaseStringUTFChars(s, cs);
			}
			break;
		case xltypeMulti:
			{
				int rows = oper->val.array.rows = env->GetIntField(o, XLOPER_ROWS_FIELD);
				int cols = oper->val.array.columns = env->GetIntField(o, XLOPER_COLS_FIELD);
				int size = rows * cols;
				jobjectArray arr = (jobjectArray) env->GetObjectField(o, XLOPER_ARRAY_FIELD);
				oper->val.array.lparray = (LPXLOPER) malloc(sizeof(XLOPER) * rows * cols);
				for(int i = 0; i < size; i++) {
					jobject o = env->GetObjectArrayElement(arr, i);
					Convert(env, o, &(oper->val.array.lparray[i]));
				}
			}
			break;
	}
}

void FreeContents(LPXLOPER px)
{
	switch(px->xltype & ~(xlbitXLFree | xlbitDLLFree)) {
		case xltypeMulti:
			for(int i = px->val.array.rows*px->val.array.columns - 1; i >=0; i--) {
				FreeContents(&px->val.array.lparray[i]);
			}
			free(px->val.array.lparray);
			break;
		case xltypeStr:
			free(px->val.str);
			break;
		default:
			break;
	}
}

int far pascal Excel4v(int xlfn, LPXLOPER operRes, int count, LPXLOPER far opers[])
{
	JNIEnv* env;
	jvm->AttachCurrentThreadAsDaemon((void**)&env, 0);
	jobject h = env->NewObject(XLOPERHOLDER_CLASS, XLOPERHOLDER_CONSTRUCTOR);
	jobjectArray a = env->NewObjectArray(count, XLOPER_CLASS, NULL);
	for(int i = 0; i < count; i++) {
		env->SetObjectArrayElement(a, i, Convert(env, opers[i]));
	}
	int res = env->CallStaticIntMethod(XLL_CLASS, XLL_EXCEL4_METHOD, xlfn, h, a);
	jobject ro = env->GetObjectField(h, XLOPERHOLDER_VALUE_FIELD);
	Convert(env, ro, operRes);
	return res;
}

int far pascal XLCallVer(void)
{
	JNIEnv* env;
	jvm->AttachCurrentThreadAsDaemon((void**)&env, 0);
	return (int) env->CallStaticIntMethod(XLL_CLASS, XLL_XLCALLVER_METHOD);
}

void CacheHandles(JNIEnv* env) 
{
	if(XLL_CLASS) return;

	// Grab the module handle for the stub
	HMODULE stubModule = GetModuleHandle("xlcall32");
	if(stubModule == NULL) {
		return;
	}

	// Attach function pointers
	typedef void (WINAPIV* SETFPFN)(void**,void**);
	SETFPFN SetFP = (SETFPFN) GetProcAddress(stubModule, "SetFunctionPointers");
	if(!SetFP)
		return;
	SetFP((void**)Excel4v, (void**)XLCallVer);

	// Cache fields for XLL class
	XLL_CLASS = (jclass) env->NewGlobalRef(env->FindClass("org/boris/jxll/JXLL"));
	XLL_XLCALLVER_METHOD = env->GetStaticMethodID(XLL_CLASS, "xlCallVer", "()I");
	XLL_EXCEL4_METHOD = env->GetStaticMethodID(XLL_CLASS, "excel4", "(ILorg/boris/jxll/XLOperHolder;[Lorg/boris/jxll/XLOper;)I");

	// Cache fields for Addin class
	ADDIN_CLASS = (jclass) env->NewGlobalRef(env->FindClass("org/boris/jxll/Addin"));
	ADDIN_LIBRARY_FIELD = env->GetFieldID(ADDIN_CLASS, "library", "J");

	// Cache fields for XLOper class
	XLOPER_CLASS = (jclass) env->NewGlobalRef(env->FindClass("org/boris/jxll/XLOper"));
	XLOPER_CONSTRUCTOR = env->GetMethodID(XLOPER_CLASS, "<init>", "()V");
	XLOPER_TYPE_FIELD = env->GetFieldID(XLOPER_CLASS, "type", "I");
	XLOPER_NUM_FIELD = env->GetFieldID(XLOPER_CLASS, "num", "D");
	XLOPER_STR_FIELD = env->GetFieldID(XLOPER_CLASS, "str", "Ljava/lang/String;");
	XLOPER_BOOL_FIELD = env->GetFieldID(XLOPER_CLASS, "bool", "Z");
	XLOPER_ERR_FIELD = env->GetFieldID(XLOPER_CLASS, "err", "I");
	XLOPER_W_FIELD = env->GetFieldID(XLOPER_CLASS, "w", "I");
	XLOPER_ROWS_FIELD = env->GetFieldID(XLOPER_CLASS, "rows", "I");
	XLOPER_COLS_FIELD = env->GetFieldID(XLOPER_CLASS, "cols", "I");
	XLOPER_ARRAY_FIELD = env->GetFieldID(XLOPER_CLASS, "array", "[Lorg/boris/jxll/XLOper;");
	XLOPER_REF_FIELD = env->GetFieldID(XLOPER_CLASS, "ref", "Lorg/boris/jxll/XLRef;");
	XLOPER_MREF_FIELD = env->GetFieldID(XLOPER_CLASS, "mref", "[Lorg/boris/jxll/XLRef;");
	XLOPER_IDSHEET_FIELD = env->GetFieldID(XLOPER_CLASS, "idSheet", "I");

	// Cache fields for XLOperHolder class
	XLOPERHOLDER_CLASS = (jclass) env->NewGlobalRef(env->FindClass("org/boris/jxll/XLOperHolder"));
	XLOPERHOLDER_CONSTRUCTOR = env->GetMethodID(XLOPERHOLDER_CLASS, "<init>", "()V");
	XLOPERHOLDER_VALUE_FIELD = env->GetFieldID(XLOPERHOLDER_CLASS, "value", "Lorg/boris/jxll/XLOper;");

	// Cache fields for XLRef class
	XLREF_CLASS = (jclass) env->NewGlobalRef(env->FindClass("org/boris/jxll/XLRef"));
	XLREF_CONSTRUCTOR = env->GetMethodID(XLREF_CLASS, "<init>", "()V");
	XLREF_RWFIRST_FIELD = env->GetFieldID(XLREF_CLASS, "rwFirst", "I");
	XLREF_RWLAST_FIELD = env->GetFieldID(XLREF_CLASS, "rwLast", "I");
	XLREF_COLFIRST_FIELD = env->GetFieldID(XLREF_CLASS, "colFirst", "I");
	XLREF_COLLAST_FIELD = env->GetFieldID(XLREF_CLASS, "colLast", "I");
}

JNIEXPORT jint JNICALL
JNI_OnLoad(JavaVM *vm, void *reserved)
{
	jvm = vm;
	JNIEnv* env;
	vm->AttachCurrentThread((void**) &env, NULL);
	CacheHandles(env);

	return JNI_VERSION_1_4;
}

JNIEXPORT jlong JNICALL 
Java_org_boris_jxll_JNI_loadLibrary(JNIEnv *env, jobject obj, jstring filename)
{
	jboolean iscopy = false;
	const char* fstr = env->GetStringUTFChars(filename, &iscopy);
	if(fstr == NULL)
		return 0;

	HINSTANCE hDLL = LoadLibrary(fstr);
	env->ReleaseStringUTFChars(filename, fstr);
	return (jlong) hDLL;
}

JNIEXPORT void JNICALL 
Java_org_boris_jxll_JNI_dispose(JNIEnv *env, jobject obj, jobject addin)
{
	if(addin==NULL) return;
	HINSTANCE hDLL = (HINSTANCE) env->GetLongField(addin, ADDIN_LIBRARY_FIELD);
	if(hDLL) {
		FreeLibrary(hDLL);
		env->SetLongField(addin, ADDIN_LIBRARY_FIELD, 0);
	}
}

JNIEXPORT void JNICALL 
Java_org_boris_jxll_JNI_xlAutoOpen(JNIEnv *env, jobject obj, jlong library)
{
	typedef int (CALLBACK* XLAUTOOPENFN)();
	XLAUTOOPENFN xlAutoOpen = (XLAUTOOPENFN) GetProcAddress((HMODULE) library, "xlAutoOpen");
	if(xlAutoOpen)
		xlAutoOpen();
}

JNIEXPORT jobject JNICALL 
Java_org_boris_jxll_JNI_xlAddInManagerInfo(JNIEnv *env, jobject obj, jlong library, jobject action)
{
	return NULL;
}

typedef union _EB {
	struct {
		DWORD lo;
		DWORD hi;
	} dw;
	double dbl;
} EB;

JNIEXPORT jobject JNICALL 
Java_org_boris_jxll_JNI_invoke(JNIEnv *env, jobject obj, jlong library, jstring function, jint returnType, jintArray argTypes, jobjectArray args)
{
	void* rdw;
	double rdo;
	XLOPER xargs[MAX_XLL_ARGS];
	int atypes[MAX_XLL_ARGS];
	jboolean iscopy=false;
	const char* fn = env->GetStringUTFChars(function, &iscopy);
	FARPROC fp = GetProcAddress((HMODULE) library, fn);
	env->ReleaseStringUTFChars(function, fn);
	if(fp==NULL)
		return NULL;
	jobject ro = env->NewObject(XLOPER_CLASS, XLOPER_CONSTRUCTOR);
	int len = env->GetArrayLength(argTypes);
	int* ar = (int *) env->GetPrimitiveArrayCritical(argTypes, &iscopy);
	memcpy(atypes, ar, sizeof(int) * len);
	env->ReleasePrimitiveArrayCritical(argTypes, ar, iscopy);
	for(int i = len-1; i >=0; i--) {
		jobject elem = env->GetObjectArrayElement(args, i);
		LPXLOPER p = &xargs[i];
		p->xltype = 0;
		switch(atypes[i]) {
			case argDouble:
				EB a;
				a.dbl = env->GetDoubleField(elem, XLOPER_NUM_FIELD);
				__asm {
					push a.dw.hi
					push a.dw.lo
				}
				break;
			case argInt:
			case argShort:
				int ai;
				double adi;
				adi = env->GetDoubleField(elem, XLOPER_NUM_FIELD);
				ai = (int) adi;
				__asm {
					push ai
				}
				break;
			case argBoolean:
				BOOL bi;
				bi = (BOOL) env->GetBooleanField(elem, XLOPER_BOOL_FIELD);
				__asm {
					mov dl, byte ptr [bi]
					push edx
				}
				break;
			case argString:
				const char* c;
				jstring s;
				s = (jstring) env->GetObjectField(elem, XLOPER_STR_FIELD);
				c = 0;
				if(s !=NULL) 
					c = env->GetStringUTFChars(s, &iscopy);
				__asm { 
					mov eax, dword ptr [c]
					push eax
				}
				break;
			case argStringLC:
				char *lcu;
				s = (jstring) env->GetObjectField(elem, XLOPER_STR_FIELD);
				c = 0;
				if(s != NULL)
					c = env->GetStringUTFChars(s, &iscopy);
				lcu = MakeExcelString(c);
				__asm { 
					mov eax, dword ptr [lcu]
					push eax
				}
				break;
			case argXLOper:
			case argXLOperRef:
				Convert(env, elem, p);
				__asm {
					push dword ptr [p]
				}
				break;
		}
	}
	__asm {
		call fp
		mov dword ptr[rdw], eax
	}
	switch(returnType) {
		case argShort:
		case argInt:
			env->SetIntField(ro, XLOPER_TYPE_FIELD, xltypeInt);
			env->SetIntField(ro, XLOPER_W_FIELD, (int) rdw);
			break;
		case argDouble:
			__asm {
				fstp rdo
			}
			env->SetIntField(ro, XLOPER_TYPE_FIELD, xltypeNum);
			env->SetDoubleField(ro, XLOPER_NUM_FIELD, rdo);
			break;
		case argString:
			env->SetIntField(ro, XLOPER_TYPE_FIELD, xltypeStr);
			if(rdw)
				env->SetObjectField(ro, XLOPER_STR_FIELD, env->NewStringUTF((const char*) rdw));
			break;
		case argXLOper:
		case argXLOperRef:
			Convert(env, (LPXLOPER) rdw, ro);
			if(rdw && ((LPXLOPER) rdw)->xltype & xlbitXLFree)
				FreeContents((LPXLOPER) rdw);
			break;
	}
	for(int i = 0; i < len; i++) 
		FreeContents(&xargs[i]);
	return ro;
}




