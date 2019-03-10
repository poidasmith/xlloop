/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#include "Runtime.h"
#include <stdio.h>

extern void _cdecl StrTruncate(LPSTR target, LPSTR source, size_t len)
{
	if(source == NULL) return;
	if(strlen(source) < len) {
		strcpy(target, source);
		return;
	}

	int i = 0;
	for(; i < len - 1; i++) {
		target[i] = source[i];
	}
	target[i] = 0;
}

extern bool _cdecl StartsWith(WCHAR* str, WCHAR* substr)
{
	return wcsncmp(str, substr, wcslen(substr)) == 0;
}

extern WCHAR* _cdecl StripArg0(WCHAR* lpCmdLine)
{
	int len = wcslen(lpCmdLine);
	int point = FindNextArg(lpCmdLine, 0, len);

	return &lpCmdLine[point];
}

extern size_t _cdecl FindNextArg(WCHAR* lpCmdLine, size_t start, size_t len)
{
	bool found = false;

	for(; start < len; start++) {
		char c = lpCmdLine[start];
		if(c == '\"') {
			found = !found;
		} else if(c == ' ') {
			if(!found) break;
		}
	}
	return start == len ? start : start + 1;
}

extern bool _cdecl StrContains(WCHAR* str, WCHAR c)
{
	unsigned int len = wcslen(str);
	for(unsigned int i = 0; i < len; i++) {
		if(c == str[i]) {
			return true;
		}
	}
	return false;
}

extern void _cdecl StrTrim(WCHAR* str, WCHAR* trimChars)
{
	unsigned int start = 0;
	unsigned int end = wcslen(str) - 1;
	for(unsigned int i = 0; i < end; i++) {
		char c = str[i];
		if(!StrContains(trimChars, c)) {
			start = i;
			break;
		}
	}
	for(int i = end; i >= 0; i--) {
		char c = str[i];
		if(!StrContains(trimChars, c)) {
			end = i;
			break;
		}
	}
	if(start != 0 || end != wcslen(str) - 1) {
		int k = 0;
		for(unsigned int i = start; i <= end; i++, k++) {
			str[k] = str[i];
		}
		str[k] = 0;
	}
}

extern void _cdecl ParseCommandLine(WCHAR* lpCmdLine, WCHAR** args, UINT& count, bool includeFirst)
{
	// Bug fix here provided by Frederic.Canut@kxen.com 
	if(lpCmdLine == NULL || *lpCmdLine == 0) return;

	StrTrim(lpCmdLine, L" ");
	int len = wcslen(lpCmdLine);
	if(len == 0) {
		return;
	}

	int start = 0;
	bool quote = false;
	bool first = true;
	TCHAR arg[4096];
	for(int i = 0; i < len; i++) {
		char c = lpCmdLine[i];
		if(c == '\"') {
			quote = !quote;
		} else if(!quote && c == ' ') {
			if(!first || includeFirst) {
				int k = 0;
				for(int j = start; j < i; j++, k++) {
					arg[k] = lpCmdLine[j];
				}
				arg[k] = 0;
				args[count] = wcsdup(arg);
				StrTrim(args[count], L" ");
				StrTrim(args[count], L"\"");
				count++;
			}
			start = i;
			first = false;
		}
	}

	// Add the last one
	if(!first || includeFirst) {
		int k = 0;
		for(int j = start; j < len; j++, k++) {
			arg[k] = lpCmdLine[j];
		}
		arg[k] = 0;
		args[count] = wcsdup(arg);
		StrTrim(args[count], L" ");
		StrTrim(args[count], L"\"");
		count++;
	}
}

extern void _cdecl GetFileDirectory(WCHAR* filename, WCHAR* output)
{
	int len = wcslen(filename);
	if(len == 0) {
		output[0] = 0;
		return;
	}
	int i = len-1;
	bool found = false;
	while(true) {
		if(filename[i] == '\\' || filename[i] == '/') {
			found = true;
			break;
		}
		if(i == 0)
			break;
		i--;
	}

	if(found) {
		i++;
		memcpy(output, filename, i);
		output[i] = 0;
	} else {
		output[0] = 0;
	}
}

extern void _cdecl GetFileName(WCHAR* filename, WCHAR* output)
{
	int len = wcslen(filename);
	if(len == 0) {
		output[0] = 0;
		return;
	}
	int i = len-1;
	bool found = false;
	while(true) {
		if(filename[i] == '\\' || filename[i] == '/') {
			found = true;
			break;
		}
		if(i == 0)
			break;
		i--;
	}

	if(found) i++;
	wcscpy(output, &filename[i]);
}

extern void _cdecl GetFileExtension(WCHAR* filename, WCHAR* output)
{
	int len = wcslen(filename);
	if(len == 0) {
		output[0] = 0;
		return;
	}
	int i = len-1;
	bool found = false;
	while(true) {
		if(filename[i] == '.') {
			found = true;
			break;
		}
		if(i == 0)
			break;
		i--;
	}

	if(found)
		wcscpy(output, &filename[i]);
	else
		output[0] = 0;
}

extern void _cdecl GetFileNameSansExtension(WCHAR* filename, WCHAR* output)
{
	int len = wcslen(filename);
	int i = len-1;
	if(len == 0) {
		output[0] = 0;
		return;
	}
	int dotPos = -1;
	while(true) {
		if(dotPos == -1 && filename[i] == '.') 
			dotPos = i;
		if(dotPos != -1 && (filename[i] == '/' || filename[i] == '\\'))
			break;
		if(i == 0)
			break;
		i--;
	}

	if(dotPos != -1) {
		if(i > 0) i++;
		memcpy(output, &filename[i], dotPos - i);
		output[dotPos - i] = 0;
	} else {
		if(i > 0) i++;
		wcscpy(output, &filename[i]);
	}
}


extern "C" void __cdecl _wassert(int e)
{
}

#ifdef TINY

extern "C" int __cdecl _purecall()
{
	return 0;
}

extern "C" int* _errno()
{
	return 0;
}

extern "C" void * __cdecl malloc(size_t size)
{
    return HeapAlloc( GetProcessHeap(), 0, size );
}

extern "C" void __cdecl free(void * p)
{
    HeapFree( GetProcessHeap(), 0, p );
}

extern "C" errno_t _cdecl strcpy_s(char *dest, rsize_t size, const char *source)
{
	strcpy(dest, source);
	return 0;
}

extern "C" int _cdecl vsprintf_s(char *buffer, size_t sizeInBytes, const char *format, va_list argptr)
{
	return vsprintf(buffer, format, argptr);
}

extern "C" int _cdecl _ftol_sse(int c)
{
	return 0;
}

extern "C" int _cdecl setvbuf(FILE* file, char* buf, int mode, size_t size)
{
	return 0;
}

extern "C" int _cdecl _ftol2_sse()
{
	return 0;
}

extern "C" FILE* _cdecl __iob_func()
{
	static FILE _iob[3] = {
	  { NULL, 0, NULL, 0, 0, 0, 0 },
	  { NULL, 0, NULL, 0, 1, 0, 0 },
	  { NULL, 0, NULL, 0, 2, 0, 0 }
	};

	return _iob;
}

extern "C" FILE* _cdecl _fdopen(int fd, const char *mode)
{
	FILE* ret = (FILE *) malloc(sizeof(FILE));
	ret->_file = fd;
	ret->_base = 0;
	ret->_cnt = 0;
	ret->_ptr = NULL;
	ret->_flag = _IOREAD | _IOWRT;
	ret->_bufsiz = 0;
	ret->_charbuf = 0;

	return ret;
}

extern "C" int _cdecl _open_osfhandle(int c)
{
	return c;
}

extern "C" int __cdecl _fileno(FILE* _File)
{
	return _File->_file;
}

HANDLE __cdecl _get_osfhandle(int _FileHandle)
{
	return (HANDLE) _FileHandle;
}

#endif 
