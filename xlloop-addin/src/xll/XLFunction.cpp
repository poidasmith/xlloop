/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "XLFunction.h"
#include "../java/VM.h"
#include "../java/JNI.h"
#include "../common/Log.h"
#include "XLObject.h"
#include "XLUtil.h"

char* StrConcat(char* lhs, char* rhs)
{
	if(lhs == NULL) return strdup(rhs);
	int len = strlen(lhs) + strlen(rhs);
	char* nu = (char *) malloc(len + 1);
	strcpy(lhs, nu);
	strcat(nu, rhs);
	return nu;
}

bool XLFunction::Initialize(JNIEnv* env)
{
	mShortcutText = "";
	mMacroType = "1";
	mHelpTopic = "";

	const char* ft = JNI::CallStringMethod(env, mFunctionClass, mFuncObj, "getName");
	if(ft == NULL) {
		return false;
	}
	mFunctionText = strdup(ft);

	const char* cat = JNI::CallStringMethod(env, mFunctionClass, mFuncObj, "getCategory");
	if(cat == NULL) {
		return false;
	}
	mCategory = strdup(cat);

	const char* ht = JNI::CallStringMethod(env, mFunctionClass, mFuncObj, "getHelpText");
	if(ht == NULL) {
		mFunctionHelp = "";
	} else {
		mFunctionHelp = strdup(ht);
	}

	mVolatile = JNI::CallBooleanMethod(env, mFunctionClass, mFuncObj, "isVolatile");

	jclass argClass = env->FindClass("org/excel4j/Argument");
	if(argClass == NULL) {
		Log::SetLastError("Could not find Argument class");
		return false;
	}

	jmethodID getArgsMethod = env->GetMethodID(mFunctionClass, "getArguments", "()[Lorg/excel4j/Argument;");
	if(getArgsMethod == NULL) {
		Log::SetLastError("Could not find Function.getArguments method");
		return false;
	}

	mArgumentText = strdup("");
	mArgumentHelp = strdup("");

	jobjectArray arr = (jobjectArray) env->CallObjectMethod(mFuncObj, getArgsMethod);
	if(arr == NULL) {
		return true;
	}

	jsize arrSize = env->GetArrayLength(arr);
	char** args = (char**) malloc(sizeof(char*) * arrSize);
	char** argHelps = (char**) malloc(sizeof(char*) * arrSize);
	for(int i = 0; i < arrSize; i++) {
		char* argName = JNI::CallStringMethod(env, argClass, env->GetObjectArrayElement(arr, i), "getName");
		char* argHelp = JNI::CallStringMethod(env, argClass, env->GetObjectArrayElement(arr, i), "getHelpText");
		args[i] = argName;
		argHelps[i] = argHelp;
	}

	for(int i = 0; i < arrSize; i++) {
		if(i > 0) {
			mArgumentText = StrConcat(mArgumentText, ",");
			mArgumentHelp = StrConcat(mArgumentHelp, ",");
		}
		mArgumentText = StrConcat(mArgumentText, args[i]);
		mArgumentHelp = StrConcat(mArgumentHelp, argHelps[i]);
	}

	for(int i = 0; i < arrSize; i++) {
		//free(args[i]);
		//free(argHelps[i]);
	}
	free(args);
	free(argHelps);

	return true;
}

LPXLOPER XLFunction::Execute(int argc, LPXLOPER argv[]) const
{
	static XLOPER err;
	err.xltype = xltypeErr;
	err.val.err = xlerrNA;
	JNIEnv* env = VM::GetJNIEnv();
	if(env == NULL) {
		return &err;
	}

	jobjectArray arr = env->NewObjectArray(argc, mObjectClass, NULL);
	for(int i = 0; i < argc; i++) {
		env->SetObjectArrayElement(arr, i, XLObject::CreateXLObject(env, argv[i]));
	}
	jobject res = env->CallObjectMethod(mFuncObj, mExecuteMethod, arr);
	if(env->ExceptionOccurred()) {
		const char* exmsg = JNI::GetExceptionMessage(env);
		if(exmsg) {
			err.xltype = xltypeStr | xlbitXLFree;
			err.val.str = XLUtil::MakeExcelString(exmsg);
			env->ExceptionClear();
		}
		return &err;
	} else if(res == NULL) {
		err.xltype = xltypeNil;
		return &err;
	} else {
		return XLObject::GetXLoper(env, res);
	}
}
