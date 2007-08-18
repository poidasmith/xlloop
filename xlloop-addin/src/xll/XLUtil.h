/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XLUTIL_H
#define XLUTIL_H

#include <windows.h>
#include <jni.h>
#include "xlcall.h"

class XLUtil {
public:
	static LPSTR MakeExcelString(const char* string);

	static int RegisterFunction(LPXLOPER xllName, 
			  const char* procedure, const char* typeText, const char* functionText,
			  const char* argumentText, const char* macroType, const char* category,
			  const char* shortcutText, const char* helpTopic, 
			  const char* functionHelp, const char* argumentHelp);

	static void CopyValue(LPXLOPER xloperSrc, LPXLOPER xloperDst);

	static bool RegisterNatives(JNIEnv* env, const char* moduleName);

	static void ThrowExcel4Exception(JNIEnv* env, int fRes);

private:
	static jobject JNICALL Excel4J(JNIEnv* env, jobject self, int xlfn, jobjectArray args);	
	static int JNICALL XLCallVerJ(JNIEnv* env, jobject self);
	static void JNICALL SetLastError(JNIEnv* env, jobject self, jstring error);
	static jstring JNICALL GetLastError(JNIEnv* env, jobject self);
	static jstring JNICALL GetModuleName(JNIEnv* env, jobject self);
};

#endif // XLUTIL_H