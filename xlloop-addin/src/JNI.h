/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef JNI_UTILS_H
#define JNI_UTILS_H

#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <jni.h>

class JNI 
{
public:
	static int StartJavaVM( TCHAR* libPath, TCHAR* vmArgs[] );
	static int RunMainClass( TCHAR* mainClass, TCHAR* progArgs[] );
	static int CleanupVM();
	static JNIEnv* GetJNIEnv();

private:
	static jstring NewJavaString(JNIEnv *env, TCHAR * str);
	static jobjectArray CreateRunArgs( JNIEnv *env, TCHAR * args[] );
};

#endif // JNI_UTILS_H
