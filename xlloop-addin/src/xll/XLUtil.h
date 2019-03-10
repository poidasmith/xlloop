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
#include <xlcall.h>

typedef struct {
	XCHAR* menuName;
	XCHAR* menuCommand;
	XCHAR* helpText;
} MENU_ITEM;

class XLUtil {
public:
	static XCHAR* MakeExcelString(const XCHAR* string);
	static LPXLOPER12 MakeExcelString2(const XCHAR* string);
	static LPXLOPER12 MakeExcelString3(XCHAR* lcstr);

	static int RegisterFunction(LPXLOPER12 xllName, 
			  const XCHAR* procedure, const XCHAR* typeText, const XCHAR* functionText,
			  const XCHAR* argumentText, const XCHAR* macroType, const XCHAR* category,
			  const XCHAR* shortcutText, const XCHAR* helpTopic,
			  const XCHAR* functionHelp, const XCHAR* argumentHelp,
			  bool command = false);

	static int RegisterCommand(LPXLOPER12 xllName, 
					  const XCHAR* procedure, const XCHAR* typeText, const XCHAR* functionText,
					  const XCHAR* argumentText, const XCHAR* macroType, const XCHAR* category,
					  const XCHAR* shortcutText);

	static int AddMenu(LPXLOPER12 xllName, MENU_ITEM* items, int itemCount, 
		XCHAR* menuPosition, XCHAR* subMenuPosition = NULL);

	static void CopyValue(LPXLOPER12 xloperSrc, LPXLOPER12 xloperDst);
	static void FreeContents(LPXLOPER12 px);
	//static char* GetCurrentSheetName();
	//static char* GetCurrentCellName();

	static void LogFunctionCall(const XCHAR* serverName, const XCHAR* name, LPXLOPER12 res, int count, ...);
	static void ToString(LPXLOPER12 px, XCHAR* dst);
	static int FindLastArg(LPXLOPER12* opers, int count);
};

class XLMap {
public:
	static LPXLOPER12 get(LPXLOPER12 pmap, const XCHAR* key);
	static XCHAR* getString(LPXLOPER12 pmap, const XCHAR* key);
	static XCHAR* getNTString(LPXLOPER12 pmap, const XCHAR* key);
	static bool getBoolean(LPXLOPER12 pmap, const XCHAR* key, const bool defValue = false);
	static int getInteger(LPXLOPER12 pmap, const XCHAR* key, const int defValue = -1);
};

#endif // XLUTIL_H