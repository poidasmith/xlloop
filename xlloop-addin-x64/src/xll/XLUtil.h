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

typedef struct {
	char* menuName;
	char* menuCommand;
	char* helpText;
} MENU_ITEM;

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

	static int AddMenu(LPXLOPER xllName, MENU_ITEM* items, int itemCount, 
		char* menuPosition, char* subMenuPosition = NULL);

	static void CopyValue(LPXLOPER xloperSrc, LPXLOPER xloperDst);
	static void FreeContents(LPXLOPER px);
	//static char* GetCurrentSheetName();
	//static char* GetCurrentCellName();

	static void LogFunctionCall(const char* serverName, const char* name, LPXLOPER res, int count, ...);
	static void ToString(LPXLOPER px, char* dst);
	static int FindLastArg(LPXLOPER* opers, int count);
};

class XLMap {
public:
	static LPXLOPER get(LPXLOPER pmap, const char* key);
	static char* getString(LPXLOPER pmap, const char* key);
	static char* getNTString(LPXLOPER pmap, const char* key);
	static bool getBoolean(LPXLOPER pmap, const char* key, const bool defValue = false);
	static int getInteger(LPXLOPER pmap, const char* key, const int defValue = -1);
};

#endif // XLUTIL_H