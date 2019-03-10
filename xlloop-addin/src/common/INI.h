/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef INI_H
#define INI_H

#include "Runtime.h"
#include "Dictionary.h"

// Internal keys
#define MODULE_NAME L"XLLoop:module.name"
#define MODULE_INI  L"XLLoop:module.ini"
#define MODULE_DIR  L"XLLoop:module.dir"
#define INI_DIR     L"XLLoop:ini.dir"

// Ini keys
#define WORKING_DIR   L":working.directory"
#define LOG_FILE      L":log"
#define LOG_LEVEL     L":log.level"
#define CLASS_PATH    L":classpath"
#define VM_ARG        L":vmarg"
#define PROG_ARG      L":arg"

class INI
{
public:
	static void GetNumberedKeysFromIni(dictionary* ini, WCHAR* keyName, WCHAR** entries, UINT& index);
	static dictionary* LoadIniFile(HINSTANCE hInstance);
	static dictionary* LoadIniFile(HINSTANCE hInstance, WCHAR* inifile);

	static WCHAR* GetString(dictionary* ini, const WCHAR* section, const WCHAR* key, WCHAR* defValue, bool defFromMainSection = true);
	static int    GetInteger(dictionary* ini, const WCHAR* section, const WCHAR* key, int defValue, bool defFromMainSection = true);
	static bool   GetBoolean(dictionary* ini, const WCHAR* section, const WCHAR* key, bool defValue, bool defFromMainSection = true);

private:
	static bool StrTrimInChars(WCHAR* trimChars, char c);
	static void StrTrim(WCHAR* str, WCHAR* trimChars);
	static void ExpandVariables(dictionary* ini);
	static void ParseRegistryKeys(dictionary* ini);
};

#endif // INI_H
