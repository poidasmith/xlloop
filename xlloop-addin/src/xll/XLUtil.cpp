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

/*
 * Make an excel string (xloper) from null-terminated string
 */
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

/*
 * Make an excel string (xloper) from len string
 */
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

/*
 * A helper function used to register a function with excel
 */
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

/*
 * Register a command with excel
 */
int XLUtil::RegisterCommand(LPXLOPER xllName, 
					  const char* procedure, const char* typeText, const char* functionText,
					  const char* argumentText, const char* macroType, const char* category,
					  const char* shortcutText)
{
	return RegisterFunction(xllName, procedure, typeText, functionText, argumentText, 
		macroType, category, shortcutText, NULL, NULL, NULL, true);
}

/*
 * Add a menu to excel
 */
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

/*
 * Mem copy from one xloper to another
 */
void XLUtil::CopyValue(LPXLOPER xloperSrc, LPXLOPER xloperDst)
{
	memcpy(xloperDst, xloperSrc, sizeof(XLOPER));
	xloperDst->xltype = (xloperSrc->xltype & ~(xlbitXLFree | xlbitDLLFree));
}

/*
 * Deep free the memory contents of an xloper
 */
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

/*
 * Log a functon call (with server name and args)
 */
void XLUtil::LogFunctionCall(const char* serverName, const char* name, LPXLOPER res, int count, ...)
{
	char buffer[MAX_LOG_LENGTH];
	buffer[0] = 0;
	if(serverName) 
		sprintf(buffer, "%s: %s(", serverName, name);
	else
		sprintf(buffer, "%s(", name);

	// Find last non-missing value
	va_list args;
	va_start(args, count);
	LPXLOPER* opers = (LPXLOPER*) args;
	count = XLUtil::FindLastArg(opers, count);
	va_end(args);
	
	// Append args as strings
	char arg[MAX_XLOPER_STR_LEN];
	for(UINT i = 0; i < count; i++) {
		XLUtil::ToString(opers[i], arg);
		int buflen = strlen(buffer);
		int arglen = strlen(arg);
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
			strcpy(&buffer[buflen+copylen], ", ");
	}
	strcat(buffer, ") = ");
	XLUtil::ToString(res, arg);
	strcat(buffer, arg);
	
	Log::Debug(buffer);
};

/*
 * Search backwards through args to find last non-missing arg.
 */
int XLUtil::FindLastArg(LPXLOPER* opers, int count)
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
void XLUtil::ToString(LPXLOPER px, char* dst)
{
	int len = 0;
	dst[0] = 0;
	char buffer[MAX_XLOPER_STR_LEN];

	switch(px->xltype & ~(xlbitXLFree | xlbitDLLFree)) {
	case xltypeBool:
		sprintf(dst, "%s", px->val.xbool ? "TRUE":"FALSE");
		break;
	case xltypeErr:
		break;
	case xltypeInt:
		sprintf(dst, "%d", px->val.w);
		break;
	case xltypeMulti:
		len = px->val.array.rows*px->val.array.columns;
		sprintf(dst, "[");
		for(UINT i = 0; i < min(len,4); i++) {
			XLUtil::ToString(&(px->val.array.lparray[i]), buffer);
			strcat(dst, buffer);
			if(i < len - 1)
				strcat(dst, ", ");
		}
		if(len>4) strcat(dst, "...");
		strcat(dst, "]");
		break;
	case xltypeNum:
		sprintf(dst, "%g", px->val.num);
		break;
	case xltypeStr:
		memcpy(dst, &px->val.str[1], (px->val.str[0]&0xff));
		dst[px->val.str[0]&0xff] = 0;
		break;
	case xltypeNil:
		break;
	case xltypeSRef:
		sprintf(dst, "{%d, %d, %d, %d}", px->val.sref.ref.colFirst, px->val.sref.ref.colLast, px->val.sref.ref.rwFirst, px->val.sref.ref.rwLast);
		break;
	case xltypeMissing:
		break;
	}
}

/*
 * Assumes a two-column array with key/value on each row
 */
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

/*
 * Attempt to get a string from a two-column array (returns as len-count string)
 */
char* XLMap::getString(LPXLOPER pmap, const char* key)
{
	LPXLOPER px = get(pmap, key);
	if(px != NULL && (px->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeStr) {
		return px->val.str;
	}
	return NULL;
}

/*
 * Attempt to get a string from a two-column array (returns a null-terminated string) 
 */
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

/*
 * Attempt to get a bool from a two-column array. Return defValue if not present.
 */
bool XLMap::getBoolean(LPXLOPER pmap, const char* key, bool defValue)
{
	LPXLOPER px = get(pmap, key);
	if(px != NULL && (px->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeBool) {
		return px->val.xbool;
	}
	return defValue;
}

/*
 * Attempt to get an integer from a two-column array. Return defValue if not present.
 */
int XLMap::getInteger(LPXLOPER pmap, const char* key, int defValue) 
{
	LPXLOPER px = get(pmap, key);
	if(px != NULL && (px->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeInt) {
		return px->val.w;
	}
	return defValue;
}
