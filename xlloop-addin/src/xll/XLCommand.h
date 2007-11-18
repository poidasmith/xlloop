/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XLCOMMAND_H
#define XLCOMMAND_H

#include "../common/Runtime.h"
#include <jni.h>
#include "xlcall.h"

class XLCommand {
public:
	XLCommand(jobject obj, jclass functionClass, jmethodID executeMethod, jclass objectClass) :
	  mFuncObj(obj), mFunctionClass(functionClass), mExecuteMethod(executeMethod), mObjectClass(objectClass) {}
	virtual ~XLCommand() {}

	bool Initialize(JNIEnv* env);

	// Provides access to the underlying object
	jobject GetFunctionObject() const {
		return mFuncObj;
	}

	// Function properties
	const char* GetFunctionText() const {
		return mFunctionText;
	}

	const char* GetArgumentText() const {
		return mArgumentText;
	}

	const char* GetMacroType() const {
		return mMacroType;
	}

	const char* GetCategory() const {
		return mCategory;
	}

	const char* GetShortcutText() const {
		return mShortcutText;
	}

	// Main execute method
	LPXLOPER Execute(int argc, LPXLOPER argv[]) const;
	
private:
	jobject mFuncObj;
	jclass mFunctionClass;
	jmethodID mExecuteMethod;
	jclass mObjectClass;
	char* mFunctionText;
	char* mArgumentText;
	char* mMacroType;
	char* mCategory;
	char* mShortcutText;
};

#endif // XLCOMMAND_H