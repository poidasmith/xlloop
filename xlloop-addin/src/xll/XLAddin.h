/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XLADDIN_H
#define XLADDIN_H

#include "XLFunction.h"
#include <jni.h>
#include <vector>

class XLAddin {
public:
	XLAddin() : mLoaded(false) {}

	bool Load(JNIEnv* env, char* addinClass);
	
	bool IsLoaded() const {
		return mLoaded;
	}

	void Close();

	int GetNumFunctions() const {
		return mFunctions.size();
	}

	const XLFunction& GetFunction(int index) const {
		return mFunctions[index];
	}
	
	const std::string& GetName() const {
		return mName;
	}
	
	const std::string& GetLongName() const {
		return mLongName;
	}
	
private:
	bool mLoaded;
	std::vector<XLFunction> mFunctions;
	std::string mName;
	std::string mLongName;
	jclass mAddinClass;
	jobject mAddinObj;
};

#endif // XLADDIN_H