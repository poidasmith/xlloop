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

#define MAX_XLOPER_STR_LEN 1024 /* max len to use when converting xloper to string */

/*
 * Make an excel string (first byte is length) from null-terminated string
 */
XCHAR* XLUtil::MakeExcelString(const XCHAR* string)
{
	if(string == NULL) return NULL;
	size_t len = lstrlenW(string);
	if(len > 255) len = 255; // Excel strings are limited to 255 chars
	char* temp = (char *) malloc(len * 2 + 3);
	memcpy(temp + 1, string, len * 2);
	temp[0] = (BYTE) len;
	temp[len * 2 + 1] = 0;
	temp[len * 2 + 2] = 0;
	return (XCHAR*) temp;
}

/*
 * Make an excel string (xloper) from null-terminated string
 */
LPXLOPER12 XLUtil::MakeExcelString2(const XCHAR* string)
{
	LPXLOPER12 xl = new XLOPER12;
	if(string == NULL) {
		xl->xltype = xltypeMissing;
	} else {
		xl->xltype = xltypeStr;
		xl->val.str = MakeExcelString(string);
	}

	return xl;
}

/*
 * Make an excel string (xloper) from len string
 */
LPXLOPER12 XLUtil::MakeExcelString3(XCHAR* lcstr) 
{
	LPXLOPER12 xl = new XLOPER12;
	if(lcstr == NULL) {
		xl->xltype = xltypeMissing;
	} else {
		xl->xltype = xltypeStr;
		xl->val.str = lcstr;
	}
	return xl;
}

/*
 * A helper function used to register a function with excel
 */
int XLUtil::RegisterFunction(LPXLOPER12 xllName, 
					  const XCHAR* procedure, const XCHAR* typeText, const XCHAR* functionText,
					  const XCHAR* argumentText, const XCHAR* macroType, const XCHAR* category,
					  const XCHAR* shortcutText, const XCHAR* helpTopic,
					  const XCHAR* functionHelp, const XCHAR* argumentHelp, bool command)
{
	XLOPER12 args[10];
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
		res = Excel12(xlfRegister, 0, 11, (LPXLOPER12) xllName, (LPXLOPER12) &args[0],
			(LPXLOPER12) &args[1], (LPXLOPER12) &args[2], (LPXLOPER12) &args[3],
			(LPXLOPER12) &args[4], (LPXLOPER12) &args[5], (LPXLOPER12) &args[6],
			(LPXLOPER12) &args[7], (LPXLOPER12) &args[8], (LPXLOPER12) &args[9]);
	} else {
		res = Excel12(xlfRegister, 0, 8, (LPXLOPER12) xllName, (LPXLOPER12) &args[0],
			(LPXLOPER12) &args[1], (LPXLOPER12) &args[2], (LPXLOPER12) &args[3],
			(LPXLOPER12) &args[4], (LPXLOPER12) &args[5], (LPXLOPER12) &args[6]);
	}

	if(res != 0) {
		Log::Error(L"Failed to register %s\n", procedure);
	}

	// Free strings
	for(int i = 0; i < 10; i++) {
		if(!args[i].val.str == NULL)
			free(args[i].val.str);
	}

	return res;
}

/*
 * Register a command with excel
 */
int XLUtil::RegisterCommand(LPXLOPER12 xllName,
					  const XCHAR* procedure, const XCHAR* typeText, const XCHAR* functionText,
					  const XCHAR* argumentText, const XCHAR* macroType, const XCHAR* category,
					  const XCHAR* shortcutText)
{
	return RegisterFunction(xllName, procedure, typeText, functionText, argumentText, 
		macroType, category, shortcutText, NULL, NULL, NULL, true);
}

/*
 * Add a menu to excel
 */
int XLUtil::AddMenu(LPXLOPER12 xllName, MENU_ITEM* items, int itemCount,
					XCHAR* menuPosition, XCHAR* subMenuPosition)
{
	HGLOBAL hMenu = GlobalAlloc( GMEM_MOVEABLE, sizeof(XLOPER12) * itemCount * 5 );
	LPXLOPER12 args = (LPXLOPER12) GlobalLock( hMenu );

	for(int i = 0; i < itemCount*5; i++)
		args[i].xltype = xltypeStr;

	for(int i = 0; i < itemCount; i++) {
		int offset = i*5;
		args[offset + 0].val.str = MakeExcelString(items[i].menuName);
		args[offset + 1].val.str = MakeExcelString(items[i].menuCommand ? items[i].menuCommand : L"");
		args[offset + 2].val.str = MakeExcelString(L"");
		args[offset + 3].val.str = MakeExcelString(items[i].helpText ? items[i].helpText : L"");
		args[offset + 4].val.str = MakeExcelString(L"");
	}

	XLOPER12 menu;
	menu.xltype = xltypeMulti;
	menu.val.array.rows = itemCount;
	menu.val.array.columns = 5;
	menu.val.array.lparray = args;

	XLOPER12 unk;
	unk.xltype = xltypeNum;
	unk.val.num = 10;

	int res = 0;

	if(menuPosition) {
		XLOPER12 menuPos;
		menuPos.xltype = xltypeStr;
		menuPos.val.str = MakeExcelString(menuPosition);

		if(subMenuPosition) {
			XLOPER12 sub;
			if((int) subMenuPosition > 1000) {
				sub.xltype = xltypeStr;
				sub.val.str = MakeExcelString(subMenuPosition);
			} else {
				sub.xltype = xltypeInt;
				sub.val.w = (int) subMenuPosition;
			}

			res = Excel12(xlfAddMenu, 0, 4, &unk, &menu, &menuPos, &sub);
		} else {
			res = Excel12(xlfAddMenu, 0, 3, &unk, &menu, &menuPos);
		}
	} else {
		res = Excel12(xlfAddMenu, 0, 2, &unk, &menu);
	}

	GlobalUnlock( hMenu );
	GlobalFree( hMenu );

	return res;
};

/*
 * Mem copy from one xloper to another
 */
void XLUtil::CopyValue(LPXLOPER12 xloperSrc, LPXLOPER12 xloperDst)
{
	memcpy(xloperDst, xloperSrc, sizeof(XLOPER12));
	xloperDst->xltype = (xloperSrc->xltype & ~(xlbitXLFree | xlbitDLLFree));
}

/*
 * Deep free the memory contents of an xloper
 */
void XLUtil::FreeContents(LPXLOPER12 px)
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

/*
 * Log a functon call (with server name and args)
 */
void XLUtil::LogFunctionCall(const XCHAR* serverName, const XCHAR* name, LPXLOPER12 res, int count, ...)
{
	XCHAR buffer[MAX_LOG_LENGTH];
	buffer[0] = 0;
	if(serverName) 
		swprintf(buffer, L"%s: %s(", serverName, name);
	else
		swprintf(buffer, L"%s(", name);

	// Find last non-missing value
	va_list args;
	va_start(args, count);
	LPXLOPER12* opers = (LPXLOPER12*) args;
	count = XLUtil::FindLastArg(opers, count);
	va_end(args);
	
	// Append args as strings
	XCHAR arg[MAX_XLOPER_STR_LEN];
	for(UINT i = 0; i < count; i++) {
		XLUtil::ToString(opers[i], arg);
		int buflen = wcslen(buffer);
		int arglen = wcslen(arg);
		int copylen = min(arglen, MAX_LOG_LENGTH-buflen-arglen-5);
		memcpy(&buffer[buflen], arg, copylen);
		buffer[buflen+copylen]=0;
		if(arglen > copylen) {
			buffer[MAX_XLOPER_STR_LEN-5]= '.';
			buffer[MAX_XLOPER_STR_LEN-4]= '.';
			buffer[MAX_XLOPER_STR_LEN-3]= '.';
			buffer[MAX_XLOPER_STR_LEN-2]= 0;
			break;
		}
		if(i < count - 1)
			wcscpy(&buffer[buflen+copylen], L", ");
	}
	wcscat_s(buffer, L") = ");
	XLUtil::ToString(res, arg);
	wcscat_s(buffer, arg);
	
	Log::Debug(buffer);
};

/*
 * Search backwards through args to find last non-missing arg.
 */
int XLUtil::FindLastArg(LPXLOPER12* opers, int count)
{
	if(!opers) return 0;

	while(count>0) {
		if((opers[count-1]->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeMissing)
			count--;
		else
			break;
	}

	return count;
}

/*
 * String representation of xloper copied to dst.
 */
void XLUtil::ToString(LPXLOPER12 px, XCHAR* dst)
{
	int len = 0;
	dst[0] = 0;
	XCHAR buffer[MAX_XLOPER_STR_LEN];

	switch(px->xltype & ~(xlbitXLFree | xlbitDLLFree)) {
	case xltypeBool:
		swprintf(dst, L"%s", px->val.xbool ? L"TRUE": L"FALSE");
		break;
	case xltypeErr:
		break;
	case xltypeInt:
		swprintf(dst, L"%d", px->val.w);
		break;
	case xltypeMulti:
		len = px->val.array.rows*px->val.array.columns;
		swprintf(dst, L"[");
		for(UINT i = 0; i < min(len,4); i++) {
			XLUtil::ToString(&(px->val.array.lparray[i]), buffer);
			wcscat(dst, buffer);
			if(i < len - 1)
				wcscat(dst, L", ");
		}
		if(len>4) wcscat(dst, L"...");
		wcscat(dst, L"]");
		break;
	case xltypeNum:
		swprintf(dst, L"%g", px->val.num);
		break;
	case xltypeStr:
		memcpy(dst, &px->val.str[1], (px->val.str[0]&0xff));
		dst[px->val.str[0]&0xff] = 0;
		break;
	case xltypeNil:
		break;
	case xltypeSRef:
		swprintf(dst, L"{%d, %d, %d, %d}", px->val.sref.ref.colFirst, px->val.sref.ref.colLast, px->val.sref.ref.rwFirst, px->val.sref.ref.rwLast);
		break;
	case xltypeMissing:
		break;
	}
}

/*
 * Assumes a two-column array with key/value on each row
 */
LPXLOPER12 XLMap::get(LPXLOPER12 pmap, const XCHAR* key)
{
	if(key == NULL) return NULL;
	int len = wcslen(key);
	int rows = pmap->val.array.rows;
	if(rows == 0) return NULL;
	int cols = pmap->val.array.columns;
	if(cols != 2) return NULL;
	int cells = rows * cols;
	for(int i =0; i < cells; i += 2) {
		LPXLOPER12 k = &pmap->val.array.lparray[i];
		if((k->xltype & ~(xlbitXLFree | xlbitDLLFree)) != xltypeStr)
			continue;
		int l = k->val.str[0];
		if(len != l)
			continue;
		if(wcsncmp(key, &k->val.str[1], l) == 0)
			return &pmap->val.array.lparray[i+1];
	}
	return NULL;
}

/*
 * Attempt to get a string from a two-column array (returns as len-count string)
 */
XCHAR* XLMap::getString(LPXLOPER12 pmap, const XCHAR* key)
{
	LPXLOPER12 px = get(pmap, key);
	if(px != NULL && (px->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeStr) {
		return px->val.str;
	}
	return NULL;
}

/*
 * Attempt to get a string from a two-column array (returns a null-terminated string) 
 */
XCHAR* XLMap::getNTString(LPXLOPER12 pmap, const XCHAR* key)
{
	XCHAR* res = getString(pmap, key);
	if(!res)
		return res;
	int len = res[0];
	XCHAR* res2 = (XCHAR*) malloc(sizeof(XCHAR) * (len+1));
	memcpy(res2, &res[1], len);
	res2[len] = 0;
	return res2;
}

/*
 * Attempt to get a bool from a two-column array. Return defValue if not present.
 */
bool XLMap::getBoolean(LPXLOPER12 pmap, const XCHAR* key, bool defValue)
{
	LPXLOPER12 px = get(pmap, key);
	if(px != NULL && (px->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeBool) {
		return px->val.xbool;
	}
	return defValue;
}

/*
 * Attempt to get an integer from a two-column array. Return defValue if not present.
 */
int XLMap::getInteger(LPXLOPER12 pmap, const XCHAR* key, int defValue) 
{
	LPXLOPER12 px = get(pmap, key);
	if(px != NULL && (px->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeInt) {
		return px->val.w;
	}
	return defValue;
}
