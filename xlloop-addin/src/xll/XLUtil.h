/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XLUTIL_H
#define XLUTIL_H

#include "../common/Runtime.h"
#include "xlcall.h"

#ifndef NO_JAVA
#include <jni.h>
#endif

class XLUtil {
public:
	static LPSTR MakeExcelString(const char* string);
	static LPXLOPER MakeExcelString2(const char* string);
	static LPXLOPER MakeExcelString3(char* lcstr);

	static int RegisterFunction(LPXLOPER xllName, 
			  const char* procedure, const char* typeText, const char* functionText,
			  const char* argumentText, const char* macroType, const char* category,
			  const char* shortcutText, const char* helpTopic, 
			  const char* functionHelp, const char* argumentHelp,
			  bool command = false);

	static int RegisterCommand(LPXLOPER xllName, 
					  const char* procedure, const char* typeText, const char* functionText,
					  const char* argumentText, const char* macroType, const char* category,
					  const char* shortcutText);

	static void CopyValue(LPXLOPER xloperSrc, LPXLOPER xloperDst);
};

class XLMap {
public:
	static LPXLOPER get(LPXLOPER pmap, const char* key);
	static char* getString(LPXLOPER pmap, const char* key);
	static bool getBoolean(LPXLOPER pmap, const char* key);
	static int getInteger(LPXLOPER pmap, const char* key);
};

#endif // XLUTIL_H