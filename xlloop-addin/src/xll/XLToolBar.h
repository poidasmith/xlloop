/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XLTOOLBAR_H
#define XLTOOLBAR_H

#include "../common/Runtime.h"
#include <jni.h>
#include "xlcall.h"

class XLToolBar {
public:
	XLToolBar(jobject obj, jclass functionClass, jclass objectClass) :
	  mFuncObj(obj), mFunctionClass(functionClass), mObjectClass(objectClass) {}
	virtual ~XLToolBar() {}

	bool Initialize(JNIEnv* env);

private:
	jobject mFuncObj;
	jclass mFunctionClass;
	jclass mObjectClass;
};

#endif // XLTOOLBAR_H