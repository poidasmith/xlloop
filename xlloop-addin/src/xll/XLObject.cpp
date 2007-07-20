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

static jclass g_XLObjectClass;
static jfieldID g_XLObjectHandle;
static jfieldID g_XLObjectType;
static std::map<int,jclass> g_XLTypeClassMap;
static std::map<int,jmethodID> g_XLTypeConstructorMap;

inline LPXLOPER XLObject::GetXLoper(JNIEnv* env, jobject xlobj)
{
	return (LPXLOPER) env->GetLongField(xlobj, g_XLObjectHandle);
}

jobject XLObject::CreateXLObject(JNIEnv* env, LPXLOPER xloper)
{
	jclass clazz = g_XLTypeClassMap[xloper->xltype & ~(xlbitXLFree | xlbitDLLFree)];
	jmethodID constructor = g_XLTypeConstructorMap[xloper->xltype & ~(xlbitXLFree | xlbitDLLFree)];
	return env->NewObject(clazz, constructor);
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
	LPXLOPER xloper = GetXLoper(env, self);
	return xloper->val.w;
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
	XLOPER xMulti, xTempMulti;
	xTempMulti.xltype = xltypeInt;
	xTempMulti.val.w = xltypeMulti;

	if ( xlretUncalced ==
			Excel4( xlCoerce, (LPXLOPER) &xMulti, 2, (LPXLOPER) xArray,
						(LPXLOPER) &xTempMulti ) )
	{
		// Throw uncalced exception
		return 0;
    }

	return CreateXLObject(env, &xMulti);
}

jstring JNICALL XLString::toString(JNIEnv* env, jobject self)
{
	LPXLOPER px = GetXLoper(env, self);
	char chars[MAX_PATH];
	strncpy_s(chars, MAX_PATH, &px->val.str[1], px->val.str[0]);
	return env->NewStringUTF(chars);
}

void XLObject::RegisterNatives(JNIEnv* env)
{
	g_XLObjectClass = env->FindClass("org/excel4j/XLObject");
	g_XLObjectHandle = env->GetFieldID(g_XLObjectClass, "handle", "L");
	g_XLObjectType = env->GetFieldID(g_XLObjectClass, "xltype", "I");

	// Build a map of xltype to wrapper class
	g_XLTypeClassMap[xltypeBigData] = env->FindClass("org/excel4j/object/XLBigData");
	g_XLTypeClassMap[xltypeBool] = env->FindClass("org/excel4j/object/XLBoolean");
	g_XLTypeClassMap[xltypeErr] = env->FindClass("org/excel4j/object/XLError");
	g_XLTypeClassMap[xltypeFlow] = env->FindClass("org/excel4j/object/XLFlow");
	g_XLTypeClassMap[xltypeInt] = env->FindClass("org/excel4j/object/XLInteger");
	g_XLTypeClassMap[xltypeMissing] = env->FindClass("org/excel4j/object/XLMissing");
	g_XLTypeClassMap[xltypeMulti] = env->FindClass("org/excel4j/object/XLArray");
	g_XLTypeClassMap[xltypeNil] = env->FindClass("org/excel4j/object/XLNil");
	g_XLTypeClassMap[xltypeRef] = env->FindClass("org/excel4j/object/XLReference");
	g_XLTypeClassMap[xltypeSRef] = env->FindClass("org/excel4j/object/XLReference");
	g_XLTypeClassMap[xltypeStr] = env->FindClass("org/excel4j/object/XLString");

	// Grab the consructor method ids for the wrapper classes
	for(std::map<int,jclass>::const_iterator i = g_XLTypeClassMap.begin(); i != g_XLTypeClassMap.end(); i++) {
		g_XLTypeConstructorMap[i->first] = env->GetMethodID(i->second, "<init>", "()V");
	}
}

