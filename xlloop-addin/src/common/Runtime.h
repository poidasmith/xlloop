/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef RUNTIME_H
#define RUNTIME_H

#define WIN32_LEAN_MEAN
#define UNICODE
#include <windows.h>

// Tags for embedded resources
#define RT_INI_FILE MAKEINTRESOURCE(687)
#define RT_JAR_FILE MAKEINTRESOURCE(688)
#define RT_SPLASH_FILE MAKEINTRESOURCE(689)
#define RES_MAGIC_SIZE 4
#define INI_RES_MAGIC MAKEFOURCC('I','N','I',' ')
#define JAR_RES_MAGIC MAKEFOURCC('J','A','R',' ')

extern WCHAR* _cdecl StripArg0(WCHAR* lpCmdLine);
extern size_t _cdecl FindNextArg(WCHAR* lpCmdLine, size_t start, size_t len);
extern bool _cdecl StartsWith(WCHAR* str, WCHAR* substr);
extern bool _cdecl StrContains(WCHAR* str, WCHAR c);
extern void _cdecl StrTrim(WCHAR* str, WCHAR* trimChars);
extern void _cdecl StrTruncate(WCHAR* target, WCHAR* source, size_t len);
extern void _cdecl ParseCommandLine(WCHAR* lpCmdLine, WCHAR** args, UINT& count, bool includeFirst = false);
extern void _cdecl GetFileDirectory(WCHAR* filename, WCHAR* output);
extern void _cdecl GetFileName(WCHAR* filename, WCHAR* output);
extern void _cdecl GetFileExtension(WCHAR* filename, WCHAR* output);
extern void _cdecl GetFileNameSansExtension(WCHAR* filename, WCHAR* output);

#endif 