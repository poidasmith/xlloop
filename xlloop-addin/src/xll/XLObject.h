/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XLOBJECT_H
#define XLOBJECT_H

#include <windows.h>
#include <jni.h>
#include "../xlcall.h"

class XLObject {
public:
	static void RegisterNatives(JNIEnv* env);
};

#endif // XLOBJECT_H