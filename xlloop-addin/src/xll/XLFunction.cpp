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
#include <vector>
#include <string>

bool XLFunction::Initialize(JNIEnv* env)
{
	mShortcutText = "";
	mMacroType = "1";
	mHelpTopic = "";

	const char* ft = JNI::CallStringMethod(env, mFunctionClass, mFuncObj, "getName");
	if(ft == NULL) {
		return false;
	}
	mFunctionText = ft;

	const char* cat = JNI::CallStringMethod(env, mFunctionClass, mFuncObj, "getCategory");
	if(cat == NULL) {
		return false;
	}
	mCategory = cat;

	const char* ht = JNI::CallStringMethod(env, mFunctionClass, mFuncObj, "getHelpText");
	if(ht == NULL) {
		mFunctionHelp = "";
	} else {
		mFunctionHelp = ht;
	}

	mVolatile = JNI::CallBooleanMethod(env, mFunctionClass, mFuncObj, "isVolatile");

	std::vector<std::string> args;
	std::vector<std::string> argHelps;

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

	jobjectArray arr = (jobjectArray) env->CallObjectMethod(mFuncObj, getArgsMethod);
	if(arr == NULL) {
		mArgumentText = "";
		mArgumentHelp = "";
		return true;
	}

	jsize arrSize = env->GetArrayLength(arr);
	for(int i = 0; i < arrSize; i++) {
		const char* argName = JNI::CallStringMethod(env, argClass, env->GetObjectArrayElement(arr, i), "getName");
		std::string argNameStr = argName == NULL ? "" : argName;
		const char* argHelp = JNI::CallStringMethod(env, argClass, env->GetObjectArrayElement(arr, i), "getHelpText");
		std::string argHelpStr = argHelp == NULL ? "" : argHelp;
		args.push_back(argNameStr);
		argHelps.push_back(argHelpStr);
	}

	for(int i = 0; i < args.size(); i++) {
		if(i > 0) {
			mArgumentText += ",";
			mArgumentHelp += ",";
		}
		mArgumentText += args[i];
		mArgumentHelp += argHelps[i];
	}

	return true;
}

LPXLOPER XLFunction::Execute(int argc, LPXLOPER argv[]) const
{
	static XLOPER err;
	err.xltype = xltypeErr;
	err.val.err = xlerrNA;
	JNIEnv* env = VM::GetJNIEnv();
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
