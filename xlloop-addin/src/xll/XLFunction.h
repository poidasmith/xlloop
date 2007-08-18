/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XLFUNCTION_H
#define XLFUNCTION_H

#include <windows.h>
#include "xlcall.h"
#include <jni.h>
#include <string>

class XLFunction {
public:
	XLFunction(jobject obj, jclass functionClass, jmethodID executeMethod, jclass objectClass) :
	  mFuncObj(obj), mFunctionClass(functionClass), mExecuteMethod(executeMethod), mObjectClass(objectClass), mVolatile(false) {}
	virtual ~XLFunction() {}

	bool Initialize(JNIEnv* env);

	// Provides access to the underlying object
	jobject GetFunctionObject() const {
		return mFuncObj;
	}

	// Function properties
	const char* GetFunctionText() const {
		return mFunctionText.c_str();
	}

	const char* GetArgumentText() const {
		return mArgumentText.c_str();
	}

	const char* GetMacroType() const {
		return mMacroType.c_str();
	}

	const char* GetCategory() const {
		return mCategory.c_str();
	}

	const char* GetShortcutText() const {
		return mShortcutText.c_str();
	}

	const char* GetHelpTopic() const {
		return mHelpTopic.c_str();
	}

	const char* GetFunctionHelp() const {
		return mFunctionHelp.c_str();
	}

	const char* GetArgumentHelp() const {
		return mArgumentHelp.c_str();
	}

	bool isVolatile() const {
		return mVolatile;
	}

	// Main execute method
	LPXLOPER Execute(int argc, LPXLOPER argv[]) const;
	
private:
	jobject mFuncObj;
	jclass mFunctionClass;
	jmethodID mExecuteMethod;
	jclass mObjectClass;
	std::string mFunctionText;
	std::string mArgumentText;
	std::string mMacroType;
	std::string mCategory;
	std::string mShortcutText;
	std::string mHelpTopic;
	std::string mFunctionHelp;
	std::string mArgumentHelp;
	bool mVolatile;
};

#endif // XLFUNCTION_H