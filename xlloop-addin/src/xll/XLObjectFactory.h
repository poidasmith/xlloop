/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XLOBJECT_FACTORY_H
#define XLOBJECT_FACTORY_H

#include <map>
#include <windows.h>
#include <jni.h>
#include "xlcall.h"

class XLObjectFactory {
public:
	static bool RegisterNatives(JNIEnv* env);

private:
	static jobject CreateArray(JNIEnv* env, jobject self, jint rows, jint columns);
	static jobject CreateBoolean(JNIEnv* env, jobject self, jboolean value);
	static jobject CreateDouble(JNIEnv* env, jobject self, jdouble value);
	static jobject CreateError(JNIEnv* env, jobject self, jint value);
	static jobject CreateInteger(JNIEnv* env, jobject self, jint value);
	static jobject CreateReference1(JNIEnv* env, jobject self, jint row, jint column);
	static jobject CreateReference2(JNIEnv* env, jobject self, jint rowFirst, jint rowLast, jint colFirst, jint colLast);
	static jobject CreateReference3(JNIEnv* env, jobject self, jint sheetId, jint rowFirst, jint rowLast, jint colFirst, jint colLast);
	static jobject CreateString(JNIEnv* env, jobject self, jstring str);
	static void Free(JNIEnv* env, jobject self, jobject xlobject);
};

#endif // XLOBJECT_FACTORY_H