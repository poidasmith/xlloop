/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XLMENU_H
#define XLMENU_H

#include "../common/Runtime.h"
#include <jni.h>
#include "xlcall.h"

class XLMenu {
public:
	XLMenu(jobject obj, jclass functionClass, jclass objectClass) :
	  mFuncObj(obj), mFunctionClass(functionClass), mObjectClass(objectClass) {}
	virtual ~XLMenu() {}

	bool Initialize(JNIEnv* env);

private:
	jobject mFuncObj;
	jclass mFunctionClass;
	jclass mObjectClass;
};

#endif // XLMENU_H