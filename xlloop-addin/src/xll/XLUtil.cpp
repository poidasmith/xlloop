/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "XLUtil.h"
#include "../common/Log.h"

// Make an excel string
LPSTR XLUtil::MakeExcelString(const char* string)
{
	if(string == NULL) return NULL;
	size_t len = strlen(string);
	if(len > 255) len = 255; // Excel strings are limited to 255 chars
	char* temp = (char *) malloc(len + 2);
	memcpy(temp + 1, string, len);
	temp[0] = (BYTE) len;
	temp[len+1] = 0;
	return temp;
}

LPXLOPER XLUtil::MakeExcelString2(const char* string)
{
	LPXLOPER xl = new XLOPER;
	if(string == NULL) {
		xl->xltype = xltypeMissing;
	} else {
		xl->xltype = xltypeStr;
		xl->val.str = MakeExcelString(string);
	}

	return xl;
}

LPXLOPER XLUtil::MakeExcelString3(char* lcstr) 
{
	LPXLOPER xl = new XLOPER;
	if(lcstr == NULL) {
		xl->xltype = xltypeMissing;
	} else {
		xl->xltype = xltypeStr;
		xl->val.str = lcstr;
	}
	return xl;
}

// A helper function used to register a function
int XLUtil::RegisterFunction(LPXLOPER xllName, 
					  const char* procedure, const char* typeText, const char* functionText,
					  const char* argumentText, const char* macroType, const char* category,
					  const char* shortcutText, const char* helpTopic, 
					  const char* functionHelp, const char* argumentHelp, bool command)
{
	XLOPER args[10];
	for(int i = 0; i < 10; i++) {
		args[i].val.str = NULL;
		args[i].xltype = xltypeStr;
	}
	args[0].val.str = MakeExcelString(procedure);
	args[1].val.str = MakeExcelString(typeText);
	args[2].val.str = MakeExcelString(functionText);
	args[3].val.str = MakeExcelString(argumentText);
	args[4].val.str = MakeExcelString(macroType);
	args[5].val.str = MakeExcelString(category);
	args[6].val.str = MakeExcelString(shortcutText);
	if(!command) {
		args[7].val.str = MakeExcelString(helpTopic);
		args[8].val.str = MakeExcelString(functionHelp);
		args[9].val.str = MakeExcelString(argumentHelp);
	}

	// Check types for NULL
	for(int i = 0; i < 10; i++) {
		if(args[i].val.str == NULL)
			args[i].xltype = xltypeMissing;
	}

	int res = 0;
	if(!command) {
		res = Excel4(xlfRegister, 0, 11, (LPXLOPER) xllName, (LPXLOPER) &args[0], 
			(LPXLOPER) &args[1], (LPXLOPER) &args[2], (LPXLOPER) &args[3],
			(LPXLOPER) &args[4], (LPXLOPER) &args[5], (LPXLOPER) &args[6], 
			(LPXLOPER) &args[7], (LPXLOPER) &args[8], (LPXLOPER) &args[9]);
	} else {
		res = Excel4(xlfRegister, 0, 8, (LPXLOPER) xllName, (LPXLOPER) &args[0], 
			(LPXLOPER) &args[1], (LPXLOPER) &args[2], (LPXLOPER) &args[3],
			(LPXLOPER) &args[4], (LPXLOPER) &args[5], (LPXLOPER) &args[6]);
	}

	if(res != 0) {
		Log::Error("Failed to register %s\n", procedure);
	}

	// Free strings
	for(int i = 0; i < 10; i++) {
		if(!args[i].val.str == NULL)
			free(args[i].val.str);
	}

	return res;
}

int XLUtil::RegisterCommand(LPXLOPER xllName, 
					  const char* procedure, const char* typeText, const char* functionText,
					  const char* argumentText, const char* macroType, const char* category,
					  const char* shortcutText)
{
	return RegisterFunction(xllName, procedure, typeText, functionText, argumentText, 
		macroType, category, shortcutText, NULL, NULL, NULL, true);
}

int XLUtil::AddMenu(LPXLOPER xllName, MENU_ITEM* items, int itemCount, 
					char* menuPosition, char* subMenuPosition)
{
	HGLOBAL hMenu = GlobalAlloc( GMEM_MOVEABLE, sizeof(XLOPER) * itemCount * 5 );
	LPXLOPER args = (LPXLOPER) GlobalLock( hMenu );

	for(int i = 0; i < itemCount*5; i++)
		args[i].xltype = xltypeStr;

	for(int i = 0; i < itemCount; i++) {
		int offset = i*5;
		args[offset + 0].val.str = MakeExcelString(items[i].menuName);
		args[offset + 1].val.str = MakeExcelString(items[i].menuCommand ? items[i].menuCommand : "");
		args[offset + 2].val.str = MakeExcelString("");
		args[offset + 3].val.str = MakeExcelString(items[i].helpText ? items[i].helpText : "");
		args[offset + 4].val.str = MakeExcelString("");
	}

	XLOPER menu;
	menu.xltype = xltypeMulti;
	menu.val.array.rows = itemCount;
	menu.val.array.columns = 5;
	menu.val.array.lparray = args;

	XLOPER unk;
	unk.xltype = xltypeNum;
	unk.val.num = 10;

	int res = 0;

	if(menuPosition) {
		XLOPER menuPos;
		menuPos.xltype = xltypeStr;
		menuPos.val.str = MakeExcelString(menuPosition);

		if(subMenuPosition) {
			XLOPER sub;
			if((int) subMenuPosition > 1000) {
				sub.xltype = xltypeStr;
				sub.val.str = MakeExcelString(subMenuPosition);
			} else {
				sub.xltype = xltypeInt;
				sub.val.w = (int) subMenuPosition;
			}

			res = Excel4(xlfAddMenu, 0, 4, &unk, &menu, &menuPos, &sub);
		} else {
			res = Excel4(xlfAddMenu, 0, 3, &unk, &menu, &menuPos);
		}
	} else {
		res = Excel4(xlfAddMenu, 0, 2, &unk, &menu);
	}

	GlobalUnlock( hMenu );
	GlobalFree( hMenu );

	return res;
};

void XLUtil::CopyValue(LPXLOPER xloperSrc, LPXLOPER xloperDst)
{
	memcpy(xloperDst, xloperSrc, sizeof(XLOPER));
	xloperDst->xltype = (xloperSrc->xltype & ~(xlbitXLFree | xlbitDLLFree));
}

void XLUtil::FreeContents(LPXLOPER px)
{
	switch(px->xltype & ~(xlbitXLFree | xlbitDLLFree)) {
		case xltypeMulti:
			for(int i = px->val.array.rows*px->val.array.columns - 1; i >=0; i--) {
				FreeContents(&px->val.array.lparray[i]);
			}
			free(px->val.array.lparray);
			break;
		case xltypeStr:
			free(px->val.str);
			break;
		default:
			break;
	}
}

// Assumes a two-column array with key/value on each row
LPXLOPER XLMap::get(LPXLOPER pmap, const char* key)
{
	if(key == NULL) return NULL;
	int len = strlen(key);
	int rows = pmap->val.array.rows;
	if(rows == 0) return NULL;
	int cols = pmap->val.array.columns;
	if(cols != 2) return NULL;
	int cells = rows * cols;
	for(int i =0; i < cells; i += 2) {
		LPXLOPER k = &pmap->val.array.lparray[i];
		if((k->xltype & ~(xlbitXLFree | xlbitDLLFree)) != xltypeStr)
			continue;
		int l = k->val.str[0];
		if(len != l)
			continue;
		if(strncmp(key, &k->val.str[1], l) == 0)
			return &pmap->val.array.lparray[i+1];
	}
	return NULL;
}

char* XLMap::getString(LPXLOPER pmap, const char* key)
{
	LPXLOPER px = get(pmap, key);
	if(px != NULL && (px->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeStr) {
		return px->val.str;
	}
	return NULL;
}

char* XLMap::getNTString(LPXLOPER pmap, const char* key)
{
	char* res = getString(pmap, key);
	if(!res)
		return res;
	int len = res[0];
	char* res2 = (char*) malloc(len+1);
	memcpy(res2, &res[1], len);
	res2[len] = 0;
	return res2;
}

bool XLMap::getBoolean(LPXLOPER pmap, const char* key)
{
	LPXLOPER px = get(pmap, key);
	if(px != NULL && (px->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeBool) {
		return px->val.xbool;
	}
	return false;
}

int XLMap::getInteger(LPXLOPER pmap, const char* key) 
{
	LPXLOPER px = get(pmap, key);
	if(px != NULL && (px->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeInt) {
		return px->val.w;
	}
	return -1;
}
