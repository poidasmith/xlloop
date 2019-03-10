/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#include "Log.h"
#include <stdio.h>
#include <fcntl.h>
#include <io.h>
#include <iostream>
#include <fstream>

namespace 
{
	BOOL haveInit = FALSE;
	BOOL canUseConsole = FALSE;
	BOOL haveConsole = FALSE;
	HANDLE g_logfileHandle = NULL;
	HANDLE g_stdHandle = NULL;
	bool g_haveLogFile = false;
	bool g_logFileAndConsole = false;
	double g_logRollSize = 0;
	WCHAR* g_logFilename = NULL;
	WCHAR* g_logRollPrefix = NULL;
	WCHAR* g_logRollSuffix = NULL;
	bool g_logOverwrite = false;
	volatile bool g_logRolling = false;
	LoggingLevel g_logLevel = none; 
	bool g_error = false;
	WCHAR g_errorText[MAX_PATH];
	bool g_logToDebugMonitor = true;
}

typedef BOOL (_stdcall *FPTR_AttachConsole) ( DWORD );

#define LOG_OVERWRITE_OPTION L":log.overwrite"
#define LOG_FILE_AND_CONSOLE L":log.file.and.console"
#define LOG_ROLL_SIZE L":log.roll.size"
#define LOG_ROLL_PREFIX L":log.roll.prefix"
#define LOG_ROLL_SUFFIX L":log.roll.suffix"
#define LOG_OUTPUT_DEBUG_MONITOR L":log.output.debug.monitor"

void Log::Init(HINSTANCE hInstance, const WCHAR* logfile, const WCHAR* loglevel, dictionary* ini)
{
	if(loglevel == NULL) {
		g_logLevel = info;
	} else if(wcscmp(loglevel, L"debug") == 0) {
		g_logLevel = debug;
	} else if(wcscmp(loglevel, L"none") == 0) {
		g_logLevel = none;
	} else if(wcscmp(loglevel, L"info") == 0) {
		g_logLevel = info;
	} else if(wcscmp(loglevel, L"warning") == 0) {
		g_logLevel = warning;
	} else if(wcscmp(loglevel, L"warn") == 0) {
		g_logLevel = warning;
	} else if(wcscmp(loglevel, L"error") == 0) {
		g_logLevel = error;
	} else if(wcscmp(loglevel, L"err") == 0) {
		g_logLevel = error;
	} else {
		g_logLevel = info;
		Warning(L"log.level unrecognized");
	}

	// Flag to indicate if we want to log to the debug monitor - useful for services
	g_logToDebugMonitor = (ini == NULL || iniparser_getboolean(ini, LOG_OUTPUT_DEBUG_MONITOR, 0));
#ifdef DEBUG_LOG
	g_logToDebugMonitor = true;
#endif

	// If there is a log file specified redirect std streams to this file
	if(logfile != NULL) {
		WCHAR defWorkingDir[MAX_PATH];
		GetCurrentDirectoryW(MAX_PATH, defWorkingDir);
		WCHAR* workingDir = iniparser_getstr(ini, WORKING_DIR);
		if(workingDir) {
			SetCurrentDirectory(iniparser_getstr(ini, INI_DIR));
			SetCurrentDirectory(workingDir);
		}
		g_logFilename = wcsdup(logfile);
		g_logOverwrite = iniparser_getboolean(ini, LOG_OVERWRITE_OPTION, false);
		g_logfileHandle = CreateFile(logfile, GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ, NULL, 
				g_logOverwrite ? CREATE_ALWAYS : OPEN_ALWAYS, 
				FILE_ATTRIBUTE_NORMAL, NULL);
		if (g_logfileHandle != INVALID_HANDLE_VALUE) {
			SetFilePointer(g_logfileHandle, 0, NULL, g_logOverwrite ? FILE_BEGIN : FILE_END);
			g_stdHandle = GetStdHandle(STD_OUTPUT_HANDLE);
			SetStdHandle(STD_OUTPUT_HANDLE, g_logfileHandle);
			SetStdHandle(STD_ERROR_HANDLE, g_logfileHandle);
			g_haveLogFile = true;
			WCHAR* logFileAndConsole = iniparser_getstr(ini, LOG_FILE_AND_CONSOLE);
			if(logFileAndConsole) {
				g_logFileAndConsole = iniparser_getboolean(ini, LOG_FILE_AND_CONSOLE, false);
			}
			// Check for log rolling
			g_logRollSize = iniparser_getdouble(ini, LOG_ROLL_SIZE, 0) * 1000000;
			if(g_logRollSize > 0) {
				WCHAR fullLog[MAX_PATH];
				WCHAR logDir[MAX_PATH];
				WCHAR logPrefix[MAX_PATH];
				WCHAR logExtension[MAX_PATH];
				GetFullPathName(logfile, MAX_PATH, fullLog, 0);
				GetFileDirectory(fullLog, logDir);
				WCHAR* prefix = iniparser_getstr(ini, LOG_ROLL_PREFIX);
				if(prefix) {
					wcscat(logDir, prefix);
				} else {
					GetFileNameSansExtension(fullLog, logPrefix);
					wcscat(logDir, logPrefix);
				}
				g_logRollPrefix = wcsdup(logDir);
				WCHAR* suffix = iniparser_getstr(ini, LOG_ROLL_SUFFIX);
				if(suffix) {
					g_logRollSuffix = wcsdup(suffix);
				} else {
					GetFileExtension(fullLog, logExtension);
					g_logRollSuffix = wcsdup(logExtension);
				}
			}
		} else {
			Log::Error(L"Could not open log file");
			g_logfileHandle = GetStdHandle(STD_OUTPUT_HANDLE);
		}
		if(workingDir) {
			SetCurrentDirectory(defWorkingDir);
		}
	} else {
		g_logfileHandle = GetStdHandle(STD_OUTPUT_HANDLE);
	}

#ifndef CONSOLE
	OSVERSIONINFO ver;
	ver.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	BOOL result = GetVersionEx(&ver);	
	if (result && ver.dwMajorVersion > 5 || (ver.dwMajorVersion == 5 && ver.dwMinorVersion > 0))
		canUseConsole = TRUE;

	if(!haveInit && !logfile) {
		if(canUseConsole) {
			// Attempt to attach to parent console (if function is present)
			HMODULE hModule = GetModuleHandle(L"kernel32");
			if(hModule != NULL) {
				FPTR_AttachConsole AttachConsole = (FPTR_AttachConsole) GetProcAddress(hModule, "AttachConsole");
				if(AttachConsole != NULL) {
					haveConsole = AttachConsole(-1);
					if(haveConsole) {
						AllocConsole();
						printf("\n\n");
					}
				}
			}
		}
		haveInit = TRUE;
	}
#endif
}

void Log::RollLog()
{
	WCHAR filename[MAX_PATH];
	SYSTEMTIME st;
	GetLocalTime(&st);
	swprintf_s(filename, MAX_PATH, L"%s-%4d%02d%02d-%02d%02d%02d%s", g_logRollPrefix, st.wYear, st.wMonth, st.wDay,
		st.wHour, st.wMinute, st.wSecond, g_logRollSuffix);
	CloseHandle(g_logfileHandle);
	MoveFileW(g_logFilename, filename);
	g_logfileHandle = CreateFile(g_logFilename, GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ, NULL, 
			g_logOverwrite ? CREATE_ALWAYS : OPEN_ALWAYS, 
			FILE_ATTRIBUTE_NORMAL, NULL);
	if (g_logfileHandle != INVALID_HANDLE_VALUE) {
		SetFilePointer(g_logfileHandle, 0, NULL, g_logOverwrite ? FILE_BEGIN : FILE_END);
	}
	Log::Info(L"Rolled log name: %s", filename);
}

/*
 * Log to console/file/debug monitor.
 * 
 * Levels are { debug = -1, info = 0, warning = 1, error = 2, none = 3 }
 */
void Log::LogIt(LoggingLevel loggingLevel, const WCHAR* marker, const WCHAR* format, va_list args)
{
	if(g_logLevel > loggingLevel) return;
	if(!format) return;

	WCHAR tmp[MAX_LOG_LENGTH];
	swprintf_s(tmp, MAX_LOG_LENGTH, format, args);
	if(g_logToDebugMonitor) {
		WCHAR tmp2[MAX_LOG_LENGTH];
		swprintf_s(tmp2, MAX_LOG_LENGTH, L"%s %s\n", marker ? marker : L"", tmp);
		OutputDebugString(tmp2);
	}
	DWORD dwRead;
	if(marker) {
		WriteFile(g_logfileHandle, marker, wcslen(marker), &dwRead, NULL);
		WriteFile(g_logfileHandle, " ", 1, &dwRead, NULL);
	}
	WriteFile(g_logfileHandle, tmp, wcslen(tmp), &dwRead, NULL);
	WriteFile(g_logfileHandle, "\r\n", 2, &dwRead, NULL);
	FlushFileBuffers(g_logfileHandle);

	// Check if we also log to console if we have a log file
	if(g_haveLogFile && g_logFileAndConsole) {
		if(marker) {
			WriteFile(g_stdHandle, marker, wcslen(marker), &dwRead, NULL);
			WriteFile(g_stdHandle, " ", 1, &dwRead, NULL);
		}
		WriteFile(g_stdHandle, tmp, wcslen(tmp), &dwRead, NULL);
		WriteFile(g_stdHandle, "\r\n", 2, &dwRead, NULL);
		FlushFileBuffers(g_stdHandle);
	}

	// Check if we need to roll the log
	if(g_logRollSize > 0 && !g_logRolling) {
		g_logRolling = true;
		DWORD size = GetFileSize(g_logfileHandle, 0);
		if(size > g_logRollSize) {
			RollLog();
		}
		g_logRolling = false;
	}
}

void Log::SetLevel(LoggingLevel loggingLevel) 
{
	g_logLevel = loggingLevel;
}

LoggingLevel Log::GetLevel()
{
	return g_logLevel;
}

void Log::SetLogFileAndConsole(bool logAndConsole)
{
	g_logFileAndConsole = logAndConsole;
}

void Log::Debug(const WCHAR* format, ...)
{
	if(g_logLevel <= debug) {
		va_list args;
		va_start(args, format);
		LogIt(info, L"[dbug]", format, args);
		va_end(args);
	}
}

void Log::Info(const WCHAR* format, ...)
{
	if(g_logLevel <= info) {
		va_list args;
		va_start(args, format);
		LogIt(info, L"[info]", format, args);
		va_end(args);
	}
}

void Log::Warning(const WCHAR* format, ...)
{
	if(g_logLevel <= warning) {
		va_list args;
		va_start(args, format);
		LogIt(warning, L"[warn]", format, args);
		va_end(args);
	}
}

void Log::Error(const WCHAR* format, ...)
{
	if(g_logLevel <= error) {
		va_list args;
		va_start(args, format);
		LogIt(error, L" [err]", format, args);
		va_end(args);
	}
}

void Log::Close() 
{
	if(g_logfileHandle) {
		CloseHandle(g_logfileHandle);
		g_logfileHandle = NULL;
	}
}

#ifndef NO_JAVA
extern "C" __declspec(dllexport) void Log_LogIt(int level, const char* marker, const char* format, ...)
{
	va_list args;
	va_start(args, format);
	Log::LogIt((LoggingLevel) level, marker, format, args);
	va_end(args);
}
#endif
