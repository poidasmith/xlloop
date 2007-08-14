/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "XLObject.h"
#include "../common/Log.h"
#include <math.h>

static jclass g_XLObjectClass;
static jfieldID g_XLObjectHandle;
static jfieldID g_XLObjectType;
static jclass g_XLTypeClassMap[12];
static jmethodID g_XLTypeConstructorMap[12];

inline LPXLOPER XLObject::GetXLoper(JNIEnv* env, jobject xlobj)
{
	return (LPXLOPER) env->GetLongField(xlobj, g_XLObjectHandle);
}

inline int GetIndex(WORD xltype)
{
	switch(xltype & ~(xlbitXLFree | xlbitDLLFree))
	{
	case xltypeBigData: return 0;
	case xltypeBool: return 1;
	case xltypeErr: return 2;
	case xltypeFlow: return 3;
	case xltypeInt: return 4;
	case xltypeMissing: return 5;
	case xltypeMulti: return 6;
	case xltypeNil: return 7;
	case xltypeNum: return 8;
	case xltypeRef: return 9;
	case xltypeSRef: return 10;
	case xltypeStr: return 11;
	}

	return -1;
}

jobject XLObject::CreateXLObject(JNIEnv* env, LPXLOPER xloper)
{
	int index = GetIndex(xloper->xltype);
	jclass clazz = g_XLTypeClassMap[index];
	jmethodID constructor = g_XLTypeConstructorMap[index];
	jobject obj = env->NewObject(clazz, constructor);
	env->SetLongField(obj, g_XLObjectHandle, (jlong) xloper);
	env->SetIntField(obj, g_XLObjectType, xloper->xltype);
	return obj;
}

jint JNICALL XLArray::rows(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.array.rows;
}

jint JNICALL XLArray::columns(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.array.columns;
}

jobject JNICALL XLArray::get(JNIEnv* env, jobject self, jint row, jint column)
{
	LPXLOPER xloper = GetXLoper(env, self);
	return CreateXLObject(env, &xloper->val.array.lparray[row * xloper->val.array.columns + column]);
}

void JNICALL XLArray::set(JNIEnv* env, jobject self, jint row, jint column, jobject value)
{
	LPXLOPER lps = GetXLoper(env, self);
	LPXLOPER lpv = GetXLoper(env, value);

	// Sanity check
	if(lps == lpv) {
		return;
	}

	int index = row * lps->val.array.columns + column;
	lps->val.array.lparray[index].xltype = lpv->xltype;

	switch(lpv->xltype & ~(xlbitXLFree | xlbitDLLFree)) {
		case xltypeStr:
			lps->val.array.lparray[index].val.str = lpv->val.str;
			break;
	}
}

jboolean JNICALL XLBoolean::toBoolean(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.boolean;
}

jint JNICALL XLError::toError(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.err;
}

jint JNICALL XLInteger::toInteger(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.w;
}

jdouble JNICALL XLNumber::toDouble(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.num;
}

jlong JNICALL XLReference::sheetID(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.mref.idSheet;
}

jint JNICALL XLReference::rowFirst(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.sref.ref.rwFirst;
}

jint JNICALL XLReference::rowLast(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.sref.ref.rwLast;
}

jint JNICALL XLReference::colFirst(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.sref.ref.colFirst;
}

jint JNICALL XLReference::colLast(JNIEnv* env, jobject self)
{
	return GetXLoper(env, self)->val.sref.ref.colLast;
}

jobject JNICALL XLReference::asArray(JNIEnv* env, jobject self)
{
	LPXLOPER xArray = GetXLoper(env, self);
	LPXLOPER xMulti = (LPXLOPER) malloc(sizeof(XLOPER));
	XLOPER xTempMulti;
	xTempMulti.xltype = xltypeInt;
	xTempMulti.val.w = xltypeMulti;

	if ( xlretUncalced ==
			Excel4( xlCoerce, (LPXLOPER) xMulti, 2, (LPXLOPER) xArray,
						(LPXLOPER) &xTempMulti ) )
	{
		// Throw uncalced exception
		free(xMulti);
		return 0;
    }

	return CreateXLObject(env, xMulti);
}

jstring JNICALL XLString::toString(JNIEnv* env, jobject self)
{
	LPXLOPER px = GetXLoper(env, self);
	char chars[MAX_PATH];
	strncpy_s(chars, MAX_PATH, &px->val.str[1], px->val.str[0]);
	return env->NewStringUTF(chars);
}

bool XLObject::RegisterNatives(JNIEnv* env)
{
	g_XLObjectClass = env->FindClass("org/excel4j/XLObject");
	if(g_XLObjectClass == NULL) {
		Log::SetLastError("Could not find XLObject in classpath.");
		return false;
	}
	
	g_XLObjectHandle = env->GetFieldID(g_XLObjectClass, "handle", "J");
	if(g_XLObjectHandle == NULL) {
		Log::SetLastError("Could not find XLObject.handle field.");
		return false;
	}
	
	g_XLObjectType = env->GetFieldID(g_XLObjectClass, "xltype", "I");
	if(g_XLObjectType == NULL) {
		Log::SetLastError("Could not find XLObject.xltype field.");
		return false;
	}

	// Build a map of xltype to wrapper class
	g_XLTypeClassMap[GetIndex(xltypeBigData)] = env->FindClass("org/excel4j/object/XLBigData");
	g_XLTypeClassMap[GetIndex(xltypeBool)] = env->FindClass("org/excel4j/object/XLBoolean");
	g_XLTypeClassMap[GetIndex(xltypeErr)] = env->FindClass("org/excel4j/object/XLError");
	g_XLTypeClassMap[GetIndex(xltypeFlow)] = env->FindClass("org/excel4j/object/XLFlow");
	g_XLTypeClassMap[GetIndex(xltypeInt)] = env->FindClass("org/excel4j/object/XLInteger");
	g_XLTypeClassMap[GetIndex(xltypeMissing)] = env->FindClass("org/excel4j/object/XLMissing");
	g_XLTypeClassMap[GetIndex(xltypeMulti)] = env->FindClass("org/excel4j/object/XLArray");
	g_XLTypeClassMap[GetIndex(xltypeNil)] = env->FindClass("org/excel4j/object/XLNil");
	g_XLTypeClassMap[GetIndex(xltypeNum)] = env->FindClass("org/excel4j/object/XLNumber");
	g_XLTypeClassMap[GetIndex(xltypeRef)] = env->FindClass("org/excel4j/object/XLReference");
	g_XLTypeClassMap[GetIndex(xltypeSRef)] = env->FindClass("org/excel4j/object/XLReference");
	g_XLTypeClassMap[GetIndex(xltypeStr)] = env->FindClass("org/excel4j/object/XLString");

	// Grab the consructor method ids for the wrapper classes
	for(int i = 0; i <= GetIndex(xltypeStr); i++) {
		if(g_XLTypeClassMap[i] == NULL) {
			return false;
		}
		g_XLTypeConstructorMap[i] = env->GetMethodID(g_XLTypeClassMap[i], "<init>", "()V");
	}

	// Register array methods
	JNINativeMethod arrayNatives[4];
	arrayNatives[0].fnPtr = XLArray::rows;
	arrayNatives[0].name = "rows";
	arrayNatives[0].signature = "()I";
	arrayNatives[1].fnPtr = XLArray::columns;
	arrayNatives[1].name = "columns";
	arrayNatives[1].signature = "()I";
	arrayNatives[2].fnPtr = XLArray::get;
	arrayNatives[2].name = "get";
	arrayNatives[2].signature = "(II)Lorg/excel4j/XLObject;";
	arrayNatives[3].fnPtr = XLArray::set;
	arrayNatives[3].name = "set";
	arrayNatives[3].signature = "(IILorg/excel4j/XLObject;)V";

	jclass clazz = env->FindClass("org/excel4j/object/XLArray");
	if(clazz == NULL) {
		Log::SetLastError("Could not find XLArray class.");
		return false;
	}

	env->RegisterNatives(clazz, arrayNatives, 4);

	// Register boolean methods
	JNINativeMethod boolNatives[1];
	boolNatives[0].fnPtr = XLBoolean::toBoolean;
	boolNatives[0].name = "asBoolean";
	boolNatives[0].signature = "()Z";

	clazz = env->FindClass("org/excel4j/object/XLBoolean");
	if(clazz == NULL) {
		Log::SetLastError("Could not find XLBoolean class.");
		return false;
	}

	env->RegisterNatives(clazz, boolNatives, 1);

	// Register error methods
	JNINativeMethod errorNatives[1];
	errorNatives[0].fnPtr = XLError::toError;
	errorNatives[0].name = "asError";
	errorNatives[0].signature = "()I";

	clazz = env->FindClass("org/excel4j/object/XLError");
	if(clazz == NULL) {
		Log::SetLastError("Could not find XLError class.");
		return false;
	}

	env->RegisterNatives(clazz, errorNatives, 1);

	// Register integer methods
	JNINativeMethod intNatives[1];
	intNatives[0].fnPtr = XLInteger::toInteger;
	intNatives[0].name = "asInteger";
	intNatives[0].signature = "()I";

	clazz = env->FindClass("org/excel4j/object/XLInteger");
	if(clazz == NULL) {
		Log::SetLastError("Could not find XLInteger class.");
		return false;
	}

	env->RegisterNatives(clazz, intNatives, 1);

	// Register number methods
	JNINativeMethod numNatives[1];
	numNatives[0].fnPtr = XLNumber::toDouble;
	numNatives[0].name = "asNumber";
	numNatives[0].signature = "()D";

	clazz = env->FindClass("org/excel4j/object/XLNumber");
	if(clazz == NULL) {
		Log::SetLastError("Could not find XLNumber class.");
		return false;
	}

	env->RegisterNatives(clazz, numNatives, 1);

	// Register string methods
	JNINativeMethod strNatives[1];
	strNatives[0].fnPtr = XLString::toString;
	strNatives[0].name = "asString";
	strNatives[0].signature = "()Ljava/lang/String;";

	clazz = env->FindClass("org/excel4j/object/XLString");
	if(clazz == NULL) {
		Log::SetLastError("Could not find XLString class.");
		return false;
	}

	env->RegisterNatives(clazz, strNatives, 1);

	// Register reference methods
	JNINativeMethod refNatives[6];
	refNatives[0].fnPtr = XLReference::sheetID;
	refNatives[0].name = "sheetId";
	refNatives[0].signature = "()J";
	refNatives[1].fnPtr = XLReference::rowFirst;
	refNatives[1].name = "rowFirst";
	refNatives[1].signature = "()I";
	refNatives[2].fnPtr = XLReference::rowLast;
	refNatives[2].name = "rowLast";
	refNatives[2].signature = "()I";
	refNatives[3].fnPtr = XLReference::colFirst;
	refNatives[3].name = "colFirst";
	refNatives[3].signature = "()I";
	refNatives[4].fnPtr = XLReference::colLast;
	refNatives[4].name = "colLast";
	refNatives[4].signature = "()I";
	refNatives[5].fnPtr = XLReference::asArray;
	refNatives[5].name = "asArray";
	refNatives[5].signature = "()Lorg/excel4j/object/XLArray;";

	clazz = env->FindClass("org/excel4j/object/XLReference");
	if(clazz == NULL) {
		Log::SetLastError("Could not find XLReference class.");
		return false;
	}

	env->RegisterNatives(clazz, refNatives, 6);

	return true;
	
}

