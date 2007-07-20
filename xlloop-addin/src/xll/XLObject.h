/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XLOBJECT_H
#define XLOBJECT_H

#include <map>
#include <windows.h>
#include <jni.h>
#include "xlcall.h"

class XLObject {
public:
	static void RegisterNatives(JNIEnv* env);
	static inline LPXLOPER GetXLoper(JNIEnv* env, jobject xlobj);
	static jobject CreateXLObject(JNIEnv* env, LPXLOPER xloper);
};

class XLArray : public XLObject {
public:
	static jint JNICALL rows(JNIEnv* env, jobject self);
	static jint JNICALL columns(JNIEnv* env, jobject self);
	static jobject JNICALL get(JNIEnv* env, jobject self, jint row, jint column);
	static void JNICALL set(JNIEnv* env, jobject self, jint row, jint column, jobject value);
};

class XLBoolean : public XLObject {
public:
	static jboolean JNICALL toBoolean(JNIEnv* env, jobject self);
};

class XLError : public XLObject {
public:
	static jint JNICALL toError(JNIEnv* env, jobject self);
};

class XLInteger : public XLObject {
public:
	static jint JNICALL toInteger(JNIEnv* env, jobject self);
};

class XLNumber : public XLObject {
public:
	static jdouble JNICALL toDouble(JNIEnv* env, jobject self);
};

class XLReference : public XLObject {
public:
	static jlong JNICALL sheetID(JNIEnv* env, jobject self);
	static jint JNICALL rowFirst(JNIEnv* env, jobject self);
	static jint JNICALL rowLast(JNIEnv* env, jobject self);
	static jint JNICALL colFirst(JNIEnv* env, jobject self);
	static jint JNICALL colLast(JNIEnv* env, jobject self);
	static jobject JNICALL asArray(JNIEnv* env, jobject self);
};

class XLString : public XLObject {
public:
	static jstring JNICALL toString(JNIEnv* env, jobject self);
};


#endif // XLOBJECT_H