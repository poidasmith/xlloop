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

	mAddinObj = env->NewObject(addinClass, ctor);
	if(mAddinObj == NULL) {
		Log::SetLastError("could not create addin class");
		return false;
	}

	// Call the getName method
	const char* addinName = JNI::CallJavaStringMethod(env, addinClass, mAddinObj, "getName");
	if(addinName == NULL) {
		return false;
	}
	mName = addinName;

	// Call the get long name method
	const char* addinLongName = JNI::CallJavaStringMethod(env, addinClass, mAddinObj, "getLongName");
	if(addinLongName == NULL) {
		return false;
	}
	mLongName = addinLongName;

	// Call the getFunctions method
	jmethodID fmeth = env->GetMethodID(addinClass, "getFunctions", "()[Lorg/excel4j/Function;");
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
		jsize flen = env->GetArrayLength(functions);
		for(int i = 0; i < flen; i++) {
			jobject obj = env->GetObjectArrayElement(functions, i);
			obj = env->NewGlobalRef(obj); // This is so the garbage collector doesn't destroy it
			XLFunction func(obj, functionClass, executeMethod, objectClass);
			bool init = func.Initialize(env);
			if(!init) {
				mFunctions.clear();
				return false;
			}
			mFunctions.push_back(func);
		}
	}

	// OK
	mLoaded = true;
	return true;
}
