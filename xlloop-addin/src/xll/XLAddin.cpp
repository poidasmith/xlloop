/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "XLAddin.h"
#include "../java/VM.h"
#include "../java/JNI.h"
#include "../common/Log.h"

bool XLAddin::Load(JNIEnv* env, char* addinClassName) 
{
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
	mAddinClass = env->FindClass(addinClassName);
	if(mAddinClass == NULL) {
		Log::SetLastError("addin class could not be found by VM");
		return false;
	}

	jmethodID ctor = env->GetMethodID(mAddinClass, "<init>",  "()V");
	if(ctor == NULL) {
		Log::SetLastError("could not find default constructor on addin class");
		return false;
	}

	mAddinObj = env->NewObject(mAddinClass, ctor);
	if(mAddinObj == NULL) {
		Log::SetLastError("could not create addin class");
		return false;
	}

	// Call the getName method
	const char* addinName = JNI::CallStringMethod(env, mAddinClass, mAddinObj, "getName");
	if(addinName == NULL) {
		return false;
	}
	mName = strdup(addinName);

	// Call the get long name method
	const char* addinLongName = JNI::CallStringMethod(env, mAddinClass, mAddinObj, "getLongName");
	if(addinLongName == NULL) {
		return false;
	}
	mLongName = strdup(addinLongName);

	// Call the getFunctions method
	jmethodID fmeth = env->GetMethodID(mAddinClass, "getFunctions", "()[Lorg/excel4j/Function;");
	if(fmeth == NULL) {
		Log::SetLastError("could not find getFunctions method");
		return false;
	}
	jobjectArray functions = (jobjectArray) env->CallObjectMethod(mAddinObj, fmeth);

	// Store a reference to the Function class
	jclass functionClass = env->FindClass("org/excel4j/Function");
	if(functionClass == NULL) {
		Log::SetLastError("Could not find Function class");
		return false;
	}
	
	// Store a reference to the Function.execute method
	jmethodID executeMethod = env->GetMethodID(functionClass, "execute", "([Lorg/excel4j/XLObject;)Lorg/excel4j/XLObject;");
	if(executeMethod == NULL) {
		Log::SetLastError("Could not find Function.execute method");
		return false;
	}

	jclass objectClass = env->FindClass("org/excel4j/XLObject");
	if(objectClass == NULL) {
		Log::SetLastError("Could not find XLObject class");
		return false;
	}

	// Add functions to our lookup table
	if(functions != NULL) {
		mFunctions = new XLFunction*[MAX_PATH];
		mNumFunctions = 0;
		jsize flen = env->GetArrayLength(functions);
		for(int i = 0; i < flen; i++) {
			jobject obj = env->GetObjectArrayElement(functions, i);
			obj = env->NewGlobalRef(obj); // This is so the garbage collector doesn't destroy it
			XLFunction* func = new XLFunction(obj, functionClass, executeMethod, objectClass);
			bool init = func->Initialize(env);
			if(!init) {
				delete [] mFunctions;
				mNumFunctions = 0;
				return false;
			}
			mFunctions[mNumFunctions++] = func;
		}
	}

	// Call the open method
	jmethodID openMethod = env->GetMethodID(mAddinClass, "open", "()V");
	if(openMethod == NULL) {
		Log::SetLastError("Could not find open method");
		return false;
	}

	env->CallVoidMethod(mAddinObj, openMethod);
	if(env->ExceptionCheck()) {
		char* exmsg = JNI::GetExceptionMessage(env);
		if(exmsg) 
			Log::SetLastError("Error calling open method: %s", exmsg);
		else
			Log::SetLastError("Error calling open method");
		return false;
	}

	// OK
	mLoaded = true;
	return true;
}

void XLAddin::Close()
{
	if(!mLoaded) {
		return;
	}

	JNIEnv* env = VM::GetJNIEnv();
	if(env == NULL) {
		return;
	}

	// Call the close method
	jmethodID closeMethod = env->GetMethodID(mAddinClass, "close", "()V");
	if(closeMethod == NULL) {
		Log::SetLastError("Could not find close method");
		return;
	}

	env->CallVoidMethod(mAddinObj, closeMethod);
	if(env->ExceptionCheck()) {
		char* exmsg = JNI::GetExceptionMessage(env);
		if(exmsg) 
			Log::SetLastError("Error calling close method: %s", exmsg);
		else
			Log::SetLastError("Error calling close method");
	}
}
