/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "common/INI.h"
#include "common/Log.h"
#include "xll/XLUtil.h"
#include "xll/xlcall.h"
#include "xll/BinaryProtocol.h"
#include "xll/HttpProtocol.h"
#include "xll/Timeout.h"
#include "xll/SheetGenerator.h"

// The DLL instance
static HINSTANCE g_hinstance = NULL;

// The INI file
static dictionary* g_ini = NULL;

// The maximum number of servers we allow
#define MAX_SERVERS 20

// The mapping of section names to server
static char* g_serverSections[MAX_SERVERS];

// The protocol manager(s)
static Protocol* g_protocol[MAX_SERVERS];
static int g_serverCount = 0;

static bool g_sendCallerInfo = false;

// The list of static function names
#define MAX_FUNCTIONS 512
static char* g_functionNames[MAX_FUNCTIONS];
static int g_functionCount = 0;

// The mapping of function index to server index
static int g_functionServer[MAX_FUNCTIONS];

// Our standard error message
static XLOPER g_errorMessage;

// Cache the log level as we will use it a lot
static bool g_debugLogging = false;

// INI keys
#define FS_PROTOCOL ":protocol"
#define FS_URL ":url"
#define FS_ADDIN_NAME ":addin.name"
#define FS_INCLUDE_GENERIC ":include.generic"
#define FS_FUNCTION_NAME ":function.name"
#define FS_INCLUDE_VOLATILE ":include.volatile"
#define FS_FUNCTION_NAME_VOLATILE ":function.name.volatile"
#define FS_FUNCTION_CATEGORY ":generic.function.category"
#define FS_DISABLE_FUNCTION_LIST ":disable.function.list"
#define FS_SEND_USER_INFO ":send.user.info"
#define FS_USER_KEY ":user.key"
#define FS_SEND_CALLER_INFO ":send.caller.info"
#define FS_PROVIDERS ":providers"

// Admin function names
#define AF_GET_FUNCTIONS "org.boris.xlloop.GetFunctions"
#define AF_INITIALIZE "org.boris.xlloop.Initialize"

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
	if(fdwReason == DLL_PROCESS_ATTACH) {
		// Store reference to handle for later use
		g_hinstance = hinstDLL;

		// Initialise the logger using std streams
		Log::Init(hinstDLL, NULL, NULL, NULL);

		// Load our (optional) ini file
		g_ini = INI::LoadIniFile(hinstDLL);

		// Init error message
		g_errorMessage.xltype = xltypeStr;
		g_errorMessage.val.str = XLUtil::MakeExcelString("#Unknown function");

		// Read config
		g_sendCallerInfo = iniparser_getboolean(g_ini, FS_SEND_CALLER_INFO, 0) != 0;

		// Initialise the log
		Log::Init(hinstDLL, iniparser_getstr(g_ini, LOG_FILE), iniparser_getstr(g_ini, LOG_LEVEL), g_ini);
		g_debugLogging = Log::GetLevel() == debug;

		// Initialise timeout feature
		Timeout::Initialise(hinstDLL, g_ini);
	} 

	// OK
	return TRUE;
}

/*
 * Sends a message to the server to indicate a new session has been created
 */
void InitializeSession(int index)
{
	bool sendUserInfo = iniparser_getboolean(g_ini, FS_SEND_USER_INFO, 1) != 0;
	if(sendUserInfo) {
		char username[MAX_PATH];
		DWORD usize= MAX_PATH;
		GetUserName(username, &usize);
		LPXLOPER uname = XLUtil::MakeExcelString2(username);
		gethostname(username, MAX_PATH);
		LPXLOPER hostname = XLUtil::MakeExcelString2(username);
		char* userKey = iniparser_getstr(g_ini, FS_USER_KEY);
		if(userKey) {
			LPXLOPER ukey = XLUtil::MakeExcelString2(userKey);
			g_protocol[index]->execute(AF_INITIALIZE, false, 3, uname, hostname, ukey);
			XLUtil::FreeContents(ukey);
			free(ukey);
		} else {
			g_protocol[index]->execute(AF_INITIALIZE, false, 2, uname, hostname);
		}
		XLUtil::FreeContents(uname);
		XLUtil::FreeContents(hostname);
		free(uname);
		free(hostname);
	}
}

/*
 * Creates the protocol handler and initiates the connection (if not already done)
 */
bool InitProtocol(int index)
{
	// Create our protocol manager
	if(g_protocol[index] == NULL) {
		char* section = g_serverSections[index];
		char* protocol = INI::GetString(g_ini, section, FS_PROTOCOL, NULL);
		if(protocol && strcmp(protocol, "http") == 0) {
			g_protocol[index] = new HttpProtocol();
		} else {
			g_protocol[index] = new BinaryProtocol();
		}
		g_protocol[index]->initialize(g_ini, section);
	}

	// Attempt connection
	bool conn = g_protocol[index]->isConnected();
	if(!conn) {
		conn = g_protocol[index]->connect() == 0;
		if(conn)
			InitializeSession(index);
	}
	return conn;
}

/*
 * Requests the function list from the server and registers each function with excel.
 */
void RegisterFunctions(LPXLOPER xDLL, int index)
{
	if(!InitProtocol(index)) {
		return;
	}

	// Ask the server for a list of functions and register them
	LPXLOPER farr = g_protocol[index]->execute(AF_GET_FUNCTIONS, false, 0);
	int t = farr->xltype & ~(xlbitXLFree | xlbitDLLFree);
	int rows = farr->val.array.rows;
	int cols = farr->val.array.columns;
	if(farr != NULL && t == xltypeMulti && cols == 1 && rows > 0) {
		for(int i = 0; i < rows; i++) {
			LPXLOPER earr = &farr->val.array.lparray[i];
			t = earr->xltype & ~(xlbitXLFree | xlbitDLLFree);
			if(t != xltypeMulti)
				continue;
			char* functionName = XLMap::getString(earr, "functionName");
			char* functionText = XLMap::getString(earr, "functionText");
			char* argumentText = XLMap::getString(earr, "argumentText");
			char* category = XLMap::getString(earr, "category");
			char* shortcutText = XLMap::getString(earr, "shortcutText");
			char* helpTopic = XLMap::getString(earr, "helpTopic");
			char* functionHelp = XLMap::getString(earr, "functionHelp");
			LPXLOPER argumentHelp = XLMap::get(earr, "argumentHelp");
			bool isVolatile = XLMap::getBoolean(earr, "isVolatile");
			if(functionName != NULL) {
				char tmp[MAX_PATH];
				sprintf(tmp, "FS%d", g_functionCount);
				char* argHelp[100];
				int argHelpCount=0;
				if(argumentHelp != NULL) {
					for(int j = 0; j < argumentHelp->val.array.rows; j++) {
						argHelp[argHelpCount++] = argumentHelp->val.array.lparray[j].val.str;
					}
					argHelp[argHelpCount++] = "";
				}
				int size = 10 + argHelpCount;
				static LPXLOPER input[20];
				input[0] = (LPXLOPER FAR) xDLL;
				input[1] = (LPXLOPER FAR) XLUtil::MakeExcelString2(tmp);
				input[2] = (LPXLOPER FAR) XLUtil::MakeExcelString2(isVolatile ? "RPPPPPPPPPPPPPPPPPPPP!" : "RPPPPPPPPPPPPPPPPPPPP");
				input[3] = (LPXLOPER FAR) XLUtil::MakeExcelString3(functionText == NULL ? functionName : functionText);
				input[4] = (LPXLOPER FAR) XLUtil::MakeExcelString3(argumentText);
				input[5] = (LPXLOPER FAR) XLUtil::MakeExcelString2("1");
				input[6] = (LPXLOPER FAR) XLUtil::MakeExcelString3(category);
				input[7] = (LPXLOPER FAR) XLUtil::MakeExcelString3(shortcutText);
				input[8] = (LPXLOPER FAR) XLUtil::MakeExcelString3(helpTopic);
				input[9] = (LPXLOPER FAR) XLUtil::MakeExcelString3(functionHelp);
				for(int j = 0; j < argHelpCount && j < 20; j++) {
					input[10 + j] = (LPXLOPER FAR) XLUtil::MakeExcelString3(argHelp[j]);
				}
				int res = Excel4v(xlfRegister, 0, size, (LPXLOPER FAR*) input);
				for(int j = 1; j < size; j++) {
					free(input[j]);
				}
				if(res == 0) {
					memcpy(tmp, &functionName[1], functionName[0]);
					tmp[functionName[0]] = 0;
					g_functionServer[g_functionCount] = index;
					g_functionNames[g_functionCount++] = strdup(tmp);

					if(g_debugLogging) {
						char* provider = g_serverSections[index];
						if(provider)
							Log::Debug("Registered function (%d) %s for server %s", g_functionCount, tmp, provider);
						else
							Log::Debug("Registered function (%d) %s", g_functionCount, tmp);
					}
				}
			}		
		}

		XLUtil::FreeContents(farr);
	}
}

/*
 * Registers all the functions & commands for a server with excel.
 */
void RegisterServer(LPXLOPER xDLL, int index)
{
	char* section = g_serverSections[index];

	bool includeGeneric = INI::GetBoolean(g_ini, section, FS_INCLUDE_GENERIC, true);
	char* function = INI::GetString(g_ini, section, FS_FUNCTION_NAME, "FS");
	char* category = INI::GetString(g_ini, section, FS_FUNCTION_CATEGORY, "General");
	bool includeVolatile = INI::GetBoolean(g_ini, section, FS_INCLUDE_VOLATILE, true);
	char* volatileFunction = INI::GetString(g_ini, section, FS_FUNCTION_NAME_VOLATILE, "FSV");

	// Register execute function (if requested)
	if(includeGeneric) {
		char fsExecuteFn[MAX_PATH];
		sprintf(fsExecuteFn, "FSExecute%d", index);
		XLUtil::RegisterFunction(xDLL, fsExecuteFn, "RCPPPPPPPPPPPPPPPPPPPP", 
			function, NULL, "1", category, NULL, NULL, NULL, NULL);
	}

	// Register execute function - volatile version (if requested)
	if(includeVolatile) {
		char fsExecuteVolFn[MAX_PATH];
		sprintf(fsExecuteVolFn, "FSExecuteVolatile%d", index);
		XLUtil::RegisterFunction(xDLL, fsExecuteVolFn, "RCPPPPPPPPPPPPPPPPPPPP!", 
			volatileFunction, NULL, "1", category, NULL, NULL, NULL, NULL);
	}

	// Now ask server for a list of functions to register
	bool disableFunctionList = INI::GetBoolean(g_ini, section, FS_DISABLE_FUNCTION_LIST, false);
	if(!disableFunctionList) {
		RegisterFunctions(xDLL, index);
	}

	// Sheet Generator - currently disabled
	//SheetGenerator::Register(xDLL, g_protocol[index], g_ini);
}

/*
 * The providers arg is a CSV of section names pointing to settings for each server
 */
void ParseProviders(char* providers)
{
	char section[MAX_PATH];
	for(int i = 0, pos = 0;; i++) {
		char c = providers[i];
		if(c == 0 || c == ',') {
			int len = i - pos;
			memcpy(section, &providers[pos], len);
			section[len] = 0;
			StrTrim(section, " ");
			g_serverSections[g_serverCount++] = strdup(section);
			pos = i + 1;
		}
		if(c == 0 || g_serverCount == MAX_SERVERS)
			break;
	}
}

#ifdef __cplusplus
extern "C" {  
#endif 

__declspec(dllexport) int WINAPI xlAutoOpen(void)
{
	static XLOPER xDLL;
	Excel4(xlGetName, &xDLL, 0);

	// Clear out and existing function names and reset function count
	g_functionCount = 0;
	g_serverCount = 0;

	// Check for providers key (a list of section names with server settings)
	char* providers = iniparser_getstring(g_ini, FS_PROVIDERS, NULL);

	if(providers) {
		// Grab the list of providers and register each
		ParseProviders(providers);
		for(int i = 0; i < g_serverCount; i++) {
			RegisterServer(&xDLL, i);
		}
	} else {
		// Backwards compatible - just the one under the main section
		g_serverCount = 1;
		g_serverSections[0] = NULL;
		RegisterServer(&xDLL, 0);
	}

	// Free the XLL filename
	Excel4(xlFree, 0, 1, (LPXLOPER) &xDLL);

	// OK
	return 1;
}

__declspec(dllexport) int WINAPI xlAutoClose(void)
{
	// Disconnect from server(s)
	for(int i = 0; i < g_serverCount; i++) {
		if(g_protocol[i] != NULL) {
			g_protocol[i]->disconnect();
			delete g_protocol[i];
			g_protocol[i] = NULL;
		}
	}

	g_serverCount = 0;

	return 1;
}

__declspec(dllexport) LPXLOPER WINAPI xlAutoRegister(LPXLOPER pxName)
{
	static XLOPER xDLL, xRegId;
	xRegId.xltype = xltypeErr;
	xRegId.val.err = xlerrValue;
	
	return (LPXLOPER) &xRegId;
}

__declspec(dllexport) int WINAPI xlAutoAdd(void)
{
	return 1;
}

__declspec(dllexport) int WINAPI xlAutoRemove(void)
{
	return 1;
}

__declspec(dllexport) void WINAPI xlAutoFree(LPXLOPER px)
{
	if(!px) return;
	XLUtil::FreeContents(px);
	free(px);
}

__declspec(dllexport) LPXLOPER WINAPI xlAddInManagerInfo(LPXLOPER xAction)
{
	static XLOPER xInfo, xIntAction, xIntType;
	xIntType.xltype = xltypeInt;
	xIntType.val.w = xltypeInt;
	xInfo.xltype = xltypeErr;
	xInfo.val.err = xlerrValue;

	Excel4(xlCoerce, &xIntAction, 2, xAction, &xIntType);

	// Set addin name
	if(xIntAction.val.w == 1) {
		LPXLOPER x = (LPXLOPER) malloc(sizeof(XLOPER));
		x->xltype = xltypeStr | xlbitDLLFree;
		char* addinName = iniparser_getstr(g_ini, FS_ADDIN_NAME);
		if(addinName == NULL) {
			char filename[MAX_PATH];
			char name[MAX_PATH];
			GetModuleFileName(g_hinstance, filename, MAX_PATH);
			GetFileNameSansExtension(filename, name);
#ifdef DEBUG_LOG
			strcat(name, " (Debug)");
#endif
			addinName = XLUtil::MakeExcelString(name);
		} else {
			addinName = XLUtil::MakeExcelString(addinName);
		}
		x->val.str = addinName;
		return x;
	} 

	return (LPXLOPER) &xInfo;
}

LPXLOPER WINAPI FSExecute(int index, const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, LPXLOPER v4, 
							LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, LPXLOPER v9, LPXLOPER v10,
							LPXLOPER v11, LPXLOPER v12, LPXLOPER v13, LPXLOPER v14, LPXLOPER v15, LPXLOPER v16,
							LPXLOPER v17, LPXLOPER v18, LPXLOPER v19)
{
	// Attempt connection
	if(!InitProtocol(index)) {
		return g_protocol[index]->getLastError();
	}

	// Exec function
	LPXLOPER xres = g_protocol[index]->execute(name, g_sendCallerInfo, 20, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19);

	// Check for error
	if(!g_protocol[index]->isConnected()) {
		if(xres) {
			XLUtil::FreeContents(xres);
			delete xres;
		}
		return g_protocol[index]->getLastError();
	}

	// Log function call
	if(g_debugLogging) {
		XLUtil::LogFunctionCall(g_serverSections[index], name, xres, 20, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19);
	}

	return xres;
}

#define DECLARE_GENERAL_FUNCTION(number) \
__declspec(dllexport) LPXLOPER WINAPI FSExecute##number (const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2 \
	,LPXLOPER v3, LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8 \
	,LPXLOPER v9, LPXLOPER v10, LPXLOPER v11, LPXLOPER v12, LPXLOPER v13, LPXLOPER v14, LPXLOPER v15, LPXLOPER v16 \
	,LPXLOPER v17, LPXLOPER v18, LPXLOPER v19) \
{ \
	return FSExecute(number, name, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19); \
} 

DECLARE_GENERAL_FUNCTION(0)
DECLARE_GENERAL_FUNCTION(1)
DECLARE_GENERAL_FUNCTION(2)
DECLARE_GENERAL_FUNCTION(3)
DECLARE_GENERAL_FUNCTION(4)
DECLARE_GENERAL_FUNCTION(5)
DECLARE_GENERAL_FUNCTION(6)
DECLARE_GENERAL_FUNCTION(7)
DECLARE_GENERAL_FUNCTION(8)
DECLARE_GENERAL_FUNCTION(9)
DECLARE_GENERAL_FUNCTION(10)
DECLARE_GENERAL_FUNCTION(11)
DECLARE_GENERAL_FUNCTION(12)
DECLARE_GENERAL_FUNCTION(13)
DECLARE_GENERAL_FUNCTION(14)
DECLARE_GENERAL_FUNCTION(15)
DECLARE_GENERAL_FUNCTION(16)
DECLARE_GENERAL_FUNCTION(17)
DECLARE_GENERAL_FUNCTION(18)
DECLARE_GENERAL_FUNCTION(19)

__declspec(dllexport) LPXLOPER WINAPI FSExecuteVolatile(int index, const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, LPXLOPER v4, 
												LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, LPXLOPER v9, LPXLOPER v10,
												LPXLOPER v11, LPXLOPER v12, LPXLOPER v13, LPXLOPER v14, LPXLOPER v15, LPXLOPER v16,
												LPXLOPER v17, LPXLOPER v18, LPXLOPER v19)
{
	// Just call off to main function (as this should have the same behaviour only volatile)
	return FSExecute(index, name, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19);
}

#define DECLARE_VOLATILE_FUNCTION(number) \
__declspec(dllexport) LPXLOPER WINAPI FSExecuteVolatile##number (const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2 \
	,LPXLOPER v3, LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8 \
	,LPXLOPER v9, LPXLOPER v10, LPXLOPER v11, LPXLOPER v12, LPXLOPER v13, LPXLOPER v14, LPXLOPER v15, LPXLOPER v16 \
	,LPXLOPER v17, LPXLOPER v18, LPXLOPER v19) \
{ \
	return FSExecuteVolatile(number, name, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19); \
} 

DECLARE_VOLATILE_FUNCTION(0)
DECLARE_VOLATILE_FUNCTION(1)
DECLARE_VOLATILE_FUNCTION(2)
DECLARE_VOLATILE_FUNCTION(3)
DECLARE_VOLATILE_FUNCTION(4)
DECLARE_VOLATILE_FUNCTION(5)
DECLARE_VOLATILE_FUNCTION(6)
DECLARE_VOLATILE_FUNCTION(7)
DECLARE_VOLATILE_FUNCTION(8)
DECLARE_VOLATILE_FUNCTION(9)
DECLARE_VOLATILE_FUNCTION(10)
DECLARE_VOLATILE_FUNCTION(11)
DECLARE_VOLATILE_FUNCTION(12)
DECLARE_VOLATILE_FUNCTION(13)
DECLARE_VOLATILE_FUNCTION(14)
DECLARE_VOLATILE_FUNCTION(15)
DECLARE_VOLATILE_FUNCTION(16)
DECLARE_VOLATILE_FUNCTION(17)
DECLARE_VOLATILE_FUNCTION(18)
DECLARE_VOLATILE_FUNCTION(19)

LPXLOPER WINAPI FSExecuteNumber(int number, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, LPXLOPER v4, 
												LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, LPXLOPER v9, LPXLOPER v10,
												LPXLOPER v11, LPXLOPER v12, LPXLOPER v13, LPXLOPER v14, LPXLOPER v15, LPXLOPER v16,
												LPXLOPER v17, LPXLOPER v18, LPXLOPER v19)
{
	if(g_functionCount < number) {
		return &g_errorMessage;
	}
	return FSExecute(g_functionServer[number], g_functionNames[number], v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19);
}

#define DECLARE_EXCEL_FUNCTION(number) \
__declspec(dllexport) LPXLOPER WINAPI FS##number (LPXLOPER v0, LPXLOPER v1, LPXLOPER v2 \
	,LPXLOPER v3, LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8 \
	,LPXLOPER v9, LPXLOPER v10, LPXLOPER v11, LPXLOPER v12, LPXLOPER v13, LPXLOPER v14, LPXLOPER v15, LPXLOPER v16 \
	,LPXLOPER v17, LPXLOPER v18, LPXLOPER v19) \
{ \
	return FSExecuteNumber(number, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19); \
} 

DECLARE_EXCEL_FUNCTION(0)
DECLARE_EXCEL_FUNCTION(1)
DECLARE_EXCEL_FUNCTION(2)
DECLARE_EXCEL_FUNCTION(3)
DECLARE_EXCEL_FUNCTION(4)
DECLARE_EXCEL_FUNCTION(5)
DECLARE_EXCEL_FUNCTION(6)
DECLARE_EXCEL_FUNCTION(7)
DECLARE_EXCEL_FUNCTION(8)
DECLARE_EXCEL_FUNCTION(9)
DECLARE_EXCEL_FUNCTION(10)
DECLARE_EXCEL_FUNCTION(11)
DECLARE_EXCEL_FUNCTION(12)
DECLARE_EXCEL_FUNCTION(13)
DECLARE_EXCEL_FUNCTION(14)
DECLARE_EXCEL_FUNCTION(15)
DECLARE_EXCEL_FUNCTION(16)
DECLARE_EXCEL_FUNCTION(17)
DECLARE_EXCEL_FUNCTION(18)
DECLARE_EXCEL_FUNCTION(19)
DECLARE_EXCEL_FUNCTION(20)
DECLARE_EXCEL_FUNCTION(21)
DECLARE_EXCEL_FUNCTION(22)
DECLARE_EXCEL_FUNCTION(23)
DECLARE_EXCEL_FUNCTION(24)
DECLARE_EXCEL_FUNCTION(25)
DECLARE_EXCEL_FUNCTION(26)
DECLARE_EXCEL_FUNCTION(27)
DECLARE_EXCEL_FUNCTION(28)
DECLARE_EXCEL_FUNCTION(29)
DECLARE_EXCEL_FUNCTION(30)
DECLARE_EXCEL_FUNCTION(31)
DECLARE_EXCEL_FUNCTION(32)
DECLARE_EXCEL_FUNCTION(33)
DECLARE_EXCEL_FUNCTION(34)
DECLARE_EXCEL_FUNCTION(35)
DECLARE_EXCEL_FUNCTION(36)
DECLARE_EXCEL_FUNCTION(37)
DECLARE_EXCEL_FUNCTION(38)
DECLARE_EXCEL_FUNCTION(39)
DECLARE_EXCEL_FUNCTION(40)
DECLARE_EXCEL_FUNCTION(41)
DECLARE_EXCEL_FUNCTION(42)
DECLARE_EXCEL_FUNCTION(43)
DECLARE_EXCEL_FUNCTION(44)
DECLARE_EXCEL_FUNCTION(45)
DECLARE_EXCEL_FUNCTION(46)
DECLARE_EXCEL_FUNCTION(47)
DECLARE_EXCEL_FUNCTION(48)
DECLARE_EXCEL_FUNCTION(49)
DECLARE_EXCEL_FUNCTION(50)
DECLARE_EXCEL_FUNCTION(51)
DECLARE_EXCEL_FUNCTION(52)
DECLARE_EXCEL_FUNCTION(53)
DECLARE_EXCEL_FUNCTION(54)
DECLARE_EXCEL_FUNCTION(55)
DECLARE_EXCEL_FUNCTION(56)
DECLARE_EXCEL_FUNCTION(57)
DECLARE_EXCEL_FUNCTION(58)
DECLARE_EXCEL_FUNCTION(59)
DECLARE_EXCEL_FUNCTION(60)
DECLARE_EXCEL_FUNCTION(61)
DECLARE_EXCEL_FUNCTION(62)
DECLARE_EXCEL_FUNCTION(63)
DECLARE_EXCEL_FUNCTION(64)
DECLARE_EXCEL_FUNCTION(65)
DECLARE_EXCEL_FUNCTION(66)
DECLARE_EXCEL_FUNCTION(67)
DECLARE_EXCEL_FUNCTION(68)
DECLARE_EXCEL_FUNCTION(69)
DECLARE_EXCEL_FUNCTION(70)
DECLARE_EXCEL_FUNCTION(71)
DECLARE_EXCEL_FUNCTION(72)
DECLARE_EXCEL_FUNCTION(73)
DECLARE_EXCEL_FUNCTION(74)
DECLARE_EXCEL_FUNCTION(75)
DECLARE_EXCEL_FUNCTION(76)
DECLARE_EXCEL_FUNCTION(77)
DECLARE_EXCEL_FUNCTION(78)
DECLARE_EXCEL_FUNCTION(79)
DECLARE_EXCEL_FUNCTION(80)
DECLARE_EXCEL_FUNCTION(81)
DECLARE_EXCEL_FUNCTION(82)
DECLARE_EXCEL_FUNCTION(83)
DECLARE_EXCEL_FUNCTION(84)
DECLARE_EXCEL_FUNCTION(85)
DECLARE_EXCEL_FUNCTION(86)
DECLARE_EXCEL_FUNCTION(87)
DECLARE_EXCEL_FUNCTION(88)
DECLARE_EXCEL_FUNCTION(89)
DECLARE_EXCEL_FUNCTION(90)
DECLARE_EXCEL_FUNCTION(91)
DECLARE_EXCEL_FUNCTION(92)
DECLARE_EXCEL_FUNCTION(93)
DECLARE_EXCEL_FUNCTION(94)
DECLARE_EXCEL_FUNCTION(95)
DECLARE_EXCEL_FUNCTION(96)
DECLARE_EXCEL_FUNCTION(97)
DECLARE_EXCEL_FUNCTION(98)
DECLARE_EXCEL_FUNCTION(99)
DECLARE_EXCEL_FUNCTION(100)
DECLARE_EXCEL_FUNCTION(101)
DECLARE_EXCEL_FUNCTION(102)
DECLARE_EXCEL_FUNCTION(103)
DECLARE_EXCEL_FUNCTION(104)
DECLARE_EXCEL_FUNCTION(105)
DECLARE_EXCEL_FUNCTION(106)
DECLARE_EXCEL_FUNCTION(107)
DECLARE_EXCEL_FUNCTION(108)
DECLARE_EXCEL_FUNCTION(109)
DECLARE_EXCEL_FUNCTION(110)
DECLARE_EXCEL_FUNCTION(111)
DECLARE_EXCEL_FUNCTION(112)
DECLARE_EXCEL_FUNCTION(113)
DECLARE_EXCEL_FUNCTION(114)
DECLARE_EXCEL_FUNCTION(115)
DECLARE_EXCEL_FUNCTION(116)
DECLARE_EXCEL_FUNCTION(117)
DECLARE_EXCEL_FUNCTION(118)
DECLARE_EXCEL_FUNCTION(119)
DECLARE_EXCEL_FUNCTION(120)
DECLARE_EXCEL_FUNCTION(121)
DECLARE_EXCEL_FUNCTION(122)
DECLARE_EXCEL_FUNCTION(123)
DECLARE_EXCEL_FUNCTION(124)
DECLARE_EXCEL_FUNCTION(125)
DECLARE_EXCEL_FUNCTION(126)
DECLARE_EXCEL_FUNCTION(127)
DECLARE_EXCEL_FUNCTION(128)
DECLARE_EXCEL_FUNCTION(129)
DECLARE_EXCEL_FUNCTION(130)
DECLARE_EXCEL_FUNCTION(131)
DECLARE_EXCEL_FUNCTION(132)
DECLARE_EXCEL_FUNCTION(133)
DECLARE_EXCEL_FUNCTION(134)
DECLARE_EXCEL_FUNCTION(135)
DECLARE_EXCEL_FUNCTION(136)
DECLARE_EXCEL_FUNCTION(137)
DECLARE_EXCEL_FUNCTION(138)
DECLARE_EXCEL_FUNCTION(139)
DECLARE_EXCEL_FUNCTION(140)
DECLARE_EXCEL_FUNCTION(141)
DECLARE_EXCEL_FUNCTION(142)
DECLARE_EXCEL_FUNCTION(143)
DECLARE_EXCEL_FUNCTION(144)
DECLARE_EXCEL_FUNCTION(145)
DECLARE_EXCEL_FUNCTION(146)
DECLARE_EXCEL_FUNCTION(147)
DECLARE_EXCEL_FUNCTION(148)
DECLARE_EXCEL_FUNCTION(149)
DECLARE_EXCEL_FUNCTION(150)
DECLARE_EXCEL_FUNCTION(151)
DECLARE_EXCEL_FUNCTION(152)
DECLARE_EXCEL_FUNCTION(153)
DECLARE_EXCEL_FUNCTION(154)
DECLARE_EXCEL_FUNCTION(155)
DECLARE_EXCEL_FUNCTION(156)
DECLARE_EXCEL_FUNCTION(157)
DECLARE_EXCEL_FUNCTION(158)
DECLARE_EXCEL_FUNCTION(159)
DECLARE_EXCEL_FUNCTION(160)
DECLARE_EXCEL_FUNCTION(161)
DECLARE_EXCEL_FUNCTION(162)
DECLARE_EXCEL_FUNCTION(163)
DECLARE_EXCEL_FUNCTION(164)
DECLARE_EXCEL_FUNCTION(165)
DECLARE_EXCEL_FUNCTION(166)
DECLARE_EXCEL_FUNCTION(167)
DECLARE_EXCEL_FUNCTION(168)
DECLARE_EXCEL_FUNCTION(169)
DECLARE_EXCEL_FUNCTION(170)
DECLARE_EXCEL_FUNCTION(171)
DECLARE_EXCEL_FUNCTION(172)
DECLARE_EXCEL_FUNCTION(173)
DECLARE_EXCEL_FUNCTION(174)
DECLARE_EXCEL_FUNCTION(175)
DECLARE_EXCEL_FUNCTION(176)
DECLARE_EXCEL_FUNCTION(177)
DECLARE_EXCEL_FUNCTION(178)
DECLARE_EXCEL_FUNCTION(179)
DECLARE_EXCEL_FUNCTION(180)
DECLARE_EXCEL_FUNCTION(181)
DECLARE_EXCEL_FUNCTION(182)
DECLARE_EXCEL_FUNCTION(183)
DECLARE_EXCEL_FUNCTION(184)
DECLARE_EXCEL_FUNCTION(185)
DECLARE_EXCEL_FUNCTION(186)
DECLARE_EXCEL_FUNCTION(187)
DECLARE_EXCEL_FUNCTION(188)
DECLARE_EXCEL_FUNCTION(189)
DECLARE_EXCEL_FUNCTION(190)
DECLARE_EXCEL_FUNCTION(191)
DECLARE_EXCEL_FUNCTION(192)
DECLARE_EXCEL_FUNCTION(193)
DECLARE_EXCEL_FUNCTION(194)
DECLARE_EXCEL_FUNCTION(195)
DECLARE_EXCEL_FUNCTION(196)
DECLARE_EXCEL_FUNCTION(197)
DECLARE_EXCEL_FUNCTION(198)
DECLARE_EXCEL_FUNCTION(199)
DECLARE_EXCEL_FUNCTION(200)
DECLARE_EXCEL_FUNCTION(201)
DECLARE_EXCEL_FUNCTION(202)
DECLARE_EXCEL_FUNCTION(203)
DECLARE_EXCEL_FUNCTION(204)
DECLARE_EXCEL_FUNCTION(205)
DECLARE_EXCEL_FUNCTION(206)
DECLARE_EXCEL_FUNCTION(207)
DECLARE_EXCEL_FUNCTION(208)
DECLARE_EXCEL_FUNCTION(209)
DECLARE_EXCEL_FUNCTION(210)
DECLARE_EXCEL_FUNCTION(211)
DECLARE_EXCEL_FUNCTION(212)
DECLARE_EXCEL_FUNCTION(213)
DECLARE_EXCEL_FUNCTION(214)
DECLARE_EXCEL_FUNCTION(215)
DECLARE_EXCEL_FUNCTION(216)
DECLARE_EXCEL_FUNCTION(217)
DECLARE_EXCEL_FUNCTION(218)
DECLARE_EXCEL_FUNCTION(219)
DECLARE_EXCEL_FUNCTION(220)
DECLARE_EXCEL_FUNCTION(221)
DECLARE_EXCEL_FUNCTION(222)
DECLARE_EXCEL_FUNCTION(223)
DECLARE_EXCEL_FUNCTION(224)
DECLARE_EXCEL_FUNCTION(225)
DECLARE_EXCEL_FUNCTION(226)
DECLARE_EXCEL_FUNCTION(227)
DECLARE_EXCEL_FUNCTION(228)
DECLARE_EXCEL_FUNCTION(229)
DECLARE_EXCEL_FUNCTION(230)
DECLARE_EXCEL_FUNCTION(231)
DECLARE_EXCEL_FUNCTION(232)
DECLARE_EXCEL_FUNCTION(233)
DECLARE_EXCEL_FUNCTION(234)
DECLARE_EXCEL_FUNCTION(235)
DECLARE_EXCEL_FUNCTION(236)
DECLARE_EXCEL_FUNCTION(237)
DECLARE_EXCEL_FUNCTION(238)
DECLARE_EXCEL_FUNCTION(239)
DECLARE_EXCEL_FUNCTION(240)
DECLARE_EXCEL_FUNCTION(241)
DECLARE_EXCEL_FUNCTION(242)
DECLARE_EXCEL_FUNCTION(243)
DECLARE_EXCEL_FUNCTION(244)
DECLARE_EXCEL_FUNCTION(245)
DECLARE_EXCEL_FUNCTION(246)
DECLARE_EXCEL_FUNCTION(247)
DECLARE_EXCEL_FUNCTION(248)
DECLARE_EXCEL_FUNCTION(249)
DECLARE_EXCEL_FUNCTION(250)
DECLARE_EXCEL_FUNCTION(251)
DECLARE_EXCEL_FUNCTION(252)
DECLARE_EXCEL_FUNCTION(253)
DECLARE_EXCEL_FUNCTION(254)
DECLARE_EXCEL_FUNCTION(255)
DECLARE_EXCEL_FUNCTION(256)
DECLARE_EXCEL_FUNCTION(257)
DECLARE_EXCEL_FUNCTION(258)
DECLARE_EXCEL_FUNCTION(259)
DECLARE_EXCEL_FUNCTION(260)
DECLARE_EXCEL_FUNCTION(261)
DECLARE_EXCEL_FUNCTION(262)
DECLARE_EXCEL_FUNCTION(263)
DECLARE_EXCEL_FUNCTION(264)
DECLARE_EXCEL_FUNCTION(265)
DECLARE_EXCEL_FUNCTION(266)
DECLARE_EXCEL_FUNCTION(267)
DECLARE_EXCEL_FUNCTION(268)
DECLARE_EXCEL_FUNCTION(269)
DECLARE_EXCEL_FUNCTION(270)
DECLARE_EXCEL_FUNCTION(271)
DECLARE_EXCEL_FUNCTION(272)
DECLARE_EXCEL_FUNCTION(273)
DECLARE_EXCEL_FUNCTION(274)
DECLARE_EXCEL_FUNCTION(275)
DECLARE_EXCEL_FUNCTION(276)
DECLARE_EXCEL_FUNCTION(277)
DECLARE_EXCEL_FUNCTION(278)
DECLARE_EXCEL_FUNCTION(279)
DECLARE_EXCEL_FUNCTION(280)
DECLARE_EXCEL_FUNCTION(281)
DECLARE_EXCEL_FUNCTION(282)
DECLARE_EXCEL_FUNCTION(283)
DECLARE_EXCEL_FUNCTION(284)
DECLARE_EXCEL_FUNCTION(285)
DECLARE_EXCEL_FUNCTION(286)
DECLARE_EXCEL_FUNCTION(287)
DECLARE_EXCEL_FUNCTION(288)
DECLARE_EXCEL_FUNCTION(289)
DECLARE_EXCEL_FUNCTION(290)
DECLARE_EXCEL_FUNCTION(291)
DECLARE_EXCEL_FUNCTION(292)
DECLARE_EXCEL_FUNCTION(293)
DECLARE_EXCEL_FUNCTION(294)
DECLARE_EXCEL_FUNCTION(295)
DECLARE_EXCEL_FUNCTION(296)
DECLARE_EXCEL_FUNCTION(297)
DECLARE_EXCEL_FUNCTION(298)
DECLARE_EXCEL_FUNCTION(299)
DECLARE_EXCEL_FUNCTION(300)
DECLARE_EXCEL_FUNCTION(301)
DECLARE_EXCEL_FUNCTION(302)
DECLARE_EXCEL_FUNCTION(303)
DECLARE_EXCEL_FUNCTION(304)
DECLARE_EXCEL_FUNCTION(305)
DECLARE_EXCEL_FUNCTION(306)
DECLARE_EXCEL_FUNCTION(307)
DECLARE_EXCEL_FUNCTION(308)
DECLARE_EXCEL_FUNCTION(309)
DECLARE_EXCEL_FUNCTION(310)
DECLARE_EXCEL_FUNCTION(311)
DECLARE_EXCEL_FUNCTION(312)
DECLARE_EXCEL_FUNCTION(313)
DECLARE_EXCEL_FUNCTION(314)
DECLARE_EXCEL_FUNCTION(315)
DECLARE_EXCEL_FUNCTION(316)
DECLARE_EXCEL_FUNCTION(317)
DECLARE_EXCEL_FUNCTION(318)
DECLARE_EXCEL_FUNCTION(319)
DECLARE_EXCEL_FUNCTION(320)
DECLARE_EXCEL_FUNCTION(321)
DECLARE_EXCEL_FUNCTION(322)
DECLARE_EXCEL_FUNCTION(323)
DECLARE_EXCEL_FUNCTION(324)
DECLARE_EXCEL_FUNCTION(325)
DECLARE_EXCEL_FUNCTION(326)
DECLARE_EXCEL_FUNCTION(327)
DECLARE_EXCEL_FUNCTION(328)
DECLARE_EXCEL_FUNCTION(329)
DECLARE_EXCEL_FUNCTION(330)
DECLARE_EXCEL_FUNCTION(331)
DECLARE_EXCEL_FUNCTION(332)
DECLARE_EXCEL_FUNCTION(333)
DECLARE_EXCEL_FUNCTION(334)
DECLARE_EXCEL_FUNCTION(335)
DECLARE_EXCEL_FUNCTION(336)
DECLARE_EXCEL_FUNCTION(337)
DECLARE_EXCEL_FUNCTION(338)
DECLARE_EXCEL_FUNCTION(339)
DECLARE_EXCEL_FUNCTION(340)
DECLARE_EXCEL_FUNCTION(341)
DECLARE_EXCEL_FUNCTION(342)
DECLARE_EXCEL_FUNCTION(343)
DECLARE_EXCEL_FUNCTION(344)
DECLARE_EXCEL_FUNCTION(345)
DECLARE_EXCEL_FUNCTION(346)
DECLARE_EXCEL_FUNCTION(347)
DECLARE_EXCEL_FUNCTION(348)
DECLARE_EXCEL_FUNCTION(349)
DECLARE_EXCEL_FUNCTION(350)
DECLARE_EXCEL_FUNCTION(351)
DECLARE_EXCEL_FUNCTION(352)
DECLARE_EXCEL_FUNCTION(353)
DECLARE_EXCEL_FUNCTION(354)
DECLARE_EXCEL_FUNCTION(355)
DECLARE_EXCEL_FUNCTION(356)
DECLARE_EXCEL_FUNCTION(357)
DECLARE_EXCEL_FUNCTION(358)
DECLARE_EXCEL_FUNCTION(359)
DECLARE_EXCEL_FUNCTION(360)
DECLARE_EXCEL_FUNCTION(361)
DECLARE_EXCEL_FUNCTION(362)
DECLARE_EXCEL_FUNCTION(363)
DECLARE_EXCEL_FUNCTION(364)
DECLARE_EXCEL_FUNCTION(365)
DECLARE_EXCEL_FUNCTION(366)
DECLARE_EXCEL_FUNCTION(367)
DECLARE_EXCEL_FUNCTION(368)
DECLARE_EXCEL_FUNCTION(369)
DECLARE_EXCEL_FUNCTION(370)
DECLARE_EXCEL_FUNCTION(371)
DECLARE_EXCEL_FUNCTION(372)
DECLARE_EXCEL_FUNCTION(373)
DECLARE_EXCEL_FUNCTION(374)
DECLARE_EXCEL_FUNCTION(375)
DECLARE_EXCEL_FUNCTION(376)
DECLARE_EXCEL_FUNCTION(377)
DECLARE_EXCEL_FUNCTION(378)
DECLARE_EXCEL_FUNCTION(379)
DECLARE_EXCEL_FUNCTION(380)
DECLARE_EXCEL_FUNCTION(381)
DECLARE_EXCEL_FUNCTION(382)
DECLARE_EXCEL_FUNCTION(383)
DECLARE_EXCEL_FUNCTION(384)
DECLARE_EXCEL_FUNCTION(385)
DECLARE_EXCEL_FUNCTION(386)
DECLARE_EXCEL_FUNCTION(387)
DECLARE_EXCEL_FUNCTION(388)
DECLARE_EXCEL_FUNCTION(389)
DECLARE_EXCEL_FUNCTION(390)
DECLARE_EXCEL_FUNCTION(391)
DECLARE_EXCEL_FUNCTION(392)
DECLARE_EXCEL_FUNCTION(393)
DECLARE_EXCEL_FUNCTION(394)
DECLARE_EXCEL_FUNCTION(395)
DECLARE_EXCEL_FUNCTION(396)
DECLARE_EXCEL_FUNCTION(397)
DECLARE_EXCEL_FUNCTION(398)
DECLARE_EXCEL_FUNCTION(399)
DECLARE_EXCEL_FUNCTION(400)
DECLARE_EXCEL_FUNCTION(401)
DECLARE_EXCEL_FUNCTION(402)
DECLARE_EXCEL_FUNCTION(403)
DECLARE_EXCEL_FUNCTION(404)
DECLARE_EXCEL_FUNCTION(405)
DECLARE_EXCEL_FUNCTION(406)
DECLARE_EXCEL_FUNCTION(407)
DECLARE_EXCEL_FUNCTION(408)
DECLARE_EXCEL_FUNCTION(409)
DECLARE_EXCEL_FUNCTION(410)
DECLARE_EXCEL_FUNCTION(411)
DECLARE_EXCEL_FUNCTION(412)
DECLARE_EXCEL_FUNCTION(413)
DECLARE_EXCEL_FUNCTION(414)
DECLARE_EXCEL_FUNCTION(415)
DECLARE_EXCEL_FUNCTION(416)
DECLARE_EXCEL_FUNCTION(417)
DECLARE_EXCEL_FUNCTION(418)
DECLARE_EXCEL_FUNCTION(419)
DECLARE_EXCEL_FUNCTION(420)
DECLARE_EXCEL_FUNCTION(421)
DECLARE_EXCEL_FUNCTION(422)
DECLARE_EXCEL_FUNCTION(423)
DECLARE_EXCEL_FUNCTION(424)
DECLARE_EXCEL_FUNCTION(425)
DECLARE_EXCEL_FUNCTION(426)
DECLARE_EXCEL_FUNCTION(427)
DECLARE_EXCEL_FUNCTION(428)
DECLARE_EXCEL_FUNCTION(429)
DECLARE_EXCEL_FUNCTION(430)
DECLARE_EXCEL_FUNCTION(431)
DECLARE_EXCEL_FUNCTION(432)
DECLARE_EXCEL_FUNCTION(433)
DECLARE_EXCEL_FUNCTION(434)
DECLARE_EXCEL_FUNCTION(435)
DECLARE_EXCEL_FUNCTION(436)
DECLARE_EXCEL_FUNCTION(437)
DECLARE_EXCEL_FUNCTION(438)
DECLARE_EXCEL_FUNCTION(439)
DECLARE_EXCEL_FUNCTION(440)
DECLARE_EXCEL_FUNCTION(441)
DECLARE_EXCEL_FUNCTION(442)
DECLARE_EXCEL_FUNCTION(443)
DECLARE_EXCEL_FUNCTION(444)
DECLARE_EXCEL_FUNCTION(445)
DECLARE_EXCEL_FUNCTION(446)
DECLARE_EXCEL_FUNCTION(447)
DECLARE_EXCEL_FUNCTION(448)
DECLARE_EXCEL_FUNCTION(449)
DECLARE_EXCEL_FUNCTION(450)
DECLARE_EXCEL_FUNCTION(451)
DECLARE_EXCEL_FUNCTION(452)
DECLARE_EXCEL_FUNCTION(453)
DECLARE_EXCEL_FUNCTION(454)
DECLARE_EXCEL_FUNCTION(455)
DECLARE_EXCEL_FUNCTION(456)
DECLARE_EXCEL_FUNCTION(457)
DECLARE_EXCEL_FUNCTION(458)
DECLARE_EXCEL_FUNCTION(459)
DECLARE_EXCEL_FUNCTION(460)
DECLARE_EXCEL_FUNCTION(461)
DECLARE_EXCEL_FUNCTION(462)
DECLARE_EXCEL_FUNCTION(463)
DECLARE_EXCEL_FUNCTION(464)
DECLARE_EXCEL_FUNCTION(465)
DECLARE_EXCEL_FUNCTION(466)
DECLARE_EXCEL_FUNCTION(467)
DECLARE_EXCEL_FUNCTION(468)
DECLARE_EXCEL_FUNCTION(469)
DECLARE_EXCEL_FUNCTION(470)
DECLARE_EXCEL_FUNCTION(471)
DECLARE_EXCEL_FUNCTION(472)
DECLARE_EXCEL_FUNCTION(473)
DECLARE_EXCEL_FUNCTION(474)
DECLARE_EXCEL_FUNCTION(475)
DECLARE_EXCEL_FUNCTION(476)
DECLARE_EXCEL_FUNCTION(477)
DECLARE_EXCEL_FUNCTION(478)
DECLARE_EXCEL_FUNCTION(479)
DECLARE_EXCEL_FUNCTION(480)
DECLARE_EXCEL_FUNCTION(481)
DECLARE_EXCEL_FUNCTION(482)
DECLARE_EXCEL_FUNCTION(483)
DECLARE_EXCEL_FUNCTION(484)
DECLARE_EXCEL_FUNCTION(485)
DECLARE_EXCEL_FUNCTION(486)
DECLARE_EXCEL_FUNCTION(487)
DECLARE_EXCEL_FUNCTION(488)
DECLARE_EXCEL_FUNCTION(489)
DECLARE_EXCEL_FUNCTION(490)
DECLARE_EXCEL_FUNCTION(491)
DECLARE_EXCEL_FUNCTION(492)
DECLARE_EXCEL_FUNCTION(493)
DECLARE_EXCEL_FUNCTION(494)
DECLARE_EXCEL_FUNCTION(495)
DECLARE_EXCEL_FUNCTION(496)
DECLARE_EXCEL_FUNCTION(497)
DECLARE_EXCEL_FUNCTION(498)
DECLARE_EXCEL_FUNCTION(499)
DECLARE_EXCEL_FUNCTION(500)
DECLARE_EXCEL_FUNCTION(501)
DECLARE_EXCEL_FUNCTION(502)
DECLARE_EXCEL_FUNCTION(503)
DECLARE_EXCEL_FUNCTION(504)
DECLARE_EXCEL_FUNCTION(505)
DECLARE_EXCEL_FUNCTION(506)
DECLARE_EXCEL_FUNCTION(507)
DECLARE_EXCEL_FUNCTION(508)
DECLARE_EXCEL_FUNCTION(509)
DECLARE_EXCEL_FUNCTION(510)
DECLARE_EXCEL_FUNCTION(511)

#ifdef __cplusplus
}
#endif