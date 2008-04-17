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
#include "common/VTCodec.h"
#include "xll/XLUtil.h"
#include "xll/xlcall.h"
#include "xll/Protocol.h"
#include "xll/XLConverter.h"

// The DLL instance
static HINSTANCE g_hinstance = NULL;

// The INI file
static dictionary* g_ini = NULL;

// The protocol manager
static Protocol* g_protocol = NULL;

// The list of static function names
static std::vector<std::string> g_functionNames;

// INI keys
#define FS_HOSTNAME ":hostname"
#define FS_PORT ":port"
#define FS_ADDIN_NAME ":addin.name"
#define FS_FUNCTION_NAME ":function.name"
#define FS_INCLUDE_VOLATILE ":include.volatile"
#define FS_FUNCTION_NAME_VOLATILE ":function.name.volatile"
#define FS_DISABLE_FUNCTION_LIST ":disable.function.list"

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
	if(fdwReason == DLL_PROCESS_ATTACH) {
		// Store reference to handle for later use
		g_hinstance = hinstDLL;

		// Load our (optional) ini file
		g_ini = INI::LoadIniFile(hinstDLL);

		// Initialise the log
		Log::Init(hinstDLL, iniparser_getstr(g_ini, LOG_FILE), iniparser_getstr(g_ini, LOG_LEVEL));
	}

	// OK
	return 1;
}

bool InitProtocol()
{
	// Create our protocol manager
	if(g_protocol == NULL) {
		char* hostname = iniparser_getstr(g_ini, FS_HOSTNAME);
		char* port = iniparser_getstr(g_ini, FS_PORT);
		g_protocol = new Protocol(hostname == NULL ? "localhost" : hostname, 
			port == NULL ? 5454 : atoi(port));
	}

	// Attempt connection
	return g_protocol->connect() == 0;
}

void RegisterFunctions()
{
	static XLOPER xDLL;

	if(!InitProtocol()) {
		return;
	}

	Excel4(xlGetName, &xDLL, 0);

	VTStruct nill;
	Variant* v = g_protocol->executeGeneric("GetFunctions", &nill);
	VTCollection* functions = NULL;
	if(v != NULL && v->getType() == VCOLLECTION)
		functions = (VTCollection*) v;
	if(functions != NULL) {
		for(int i = 0, findex = 0; i < functions->size() & i < 256; i++) {
			VTStruct* str = functions->getStruct(i);
			if(str != NULL) {
				const char* functionName = str->getString("functionName");
				const char* functionText = str->getString("functionText");
				const char* argumentText = str->getString("argumentText");
				const char* category = str->getString("category");
				const char* shortcutText = str->getString("shortcutText");
				const char* helpTopic = str->getString("helpTopic");
				const char* functionHelp = str->getString("functionHelp");
				VTCollection* argumentHelp = str->getCollection("argumentHelp");
				bool isVolatile = str->getBoolean("isVolatile");
				if(functionName != NULL) {
					char tmp[MAX_PATH];
					sprintf(tmp, "FS%d", findex);
					std::vector<const char*> argHelp;
					if(argumentHelp != NULL) {
						for(int j = 0; j < argumentHelp->size(); j++) {
							argHelp.push_back(argumentHelp->getString(j));
						}
						argHelp.push_back("");
					}
					int size = 10 + argHelp.size();
					static LPXLOPER input[20];
					input[0] = (LPXLOPER FAR) &xDLL;
					input[1] = (LPXLOPER FAR) XLUtil::MakeExcelString2(tmp);
					input[2] = (LPXLOPER FAR) XLUtil::MakeExcelString2(isVolatile ? "PPPPPPPPPPP!" : "PPPPPPPPPPP");
					input[3] = (LPXLOPER FAR) XLUtil::MakeExcelString2(functionText == NULL ? functionName : functionText);
					input[4] = (LPXLOPER FAR) XLUtil::MakeExcelString2(argumentText);
					input[5] = (LPXLOPER FAR) XLUtil::MakeExcelString2("1");
					input[6] = (LPXLOPER FAR) XLUtil::MakeExcelString2(category);
					input[7] = (LPXLOPER FAR) XLUtil::MakeExcelString2(shortcutText);
					input[8] = (LPXLOPER FAR) XLUtil::MakeExcelString2(helpTopic);
					input[9] = (LPXLOPER FAR) XLUtil::MakeExcelString2(functionHelp);
					for(int j = 0; j < argHelp.size() && j < 20; j++) {
						input[10 + j] = (LPXLOPER FAR) XLUtil::MakeExcelString2(argHelp[j]);
					}
					int res = Excel4v(xlfRegister, 0, size, (LPXLOPER FAR*) input);

					for(int j = 1; j < size; j++) {
						if(input[j]->xltype == xltypeStr && input[j]->val.str != NULL) {
							free(input[j]->val.str);
						}
						free(input[j]);
					}
					if(res == 0) {
						g_functionNames.push_back(functionName);
						findex++;
					}
				}
			}
		}
	}

	// Free the XLL filename
	Excel4(xlFree, 0, 1, (LPXLOPER) &xDLL);

	if(v != NULL) delete v;
}

#ifdef __cplusplus
extern "C" {  
#endif 

__declspec(dllexport) int WINAPI xlAutoOpen(void)
{
	static XLOPER xDLL;
	Excel4(xlGetName, &xDLL, 0);

	// Register execute function
	char* fsName = iniparser_getstr(g_ini, FS_FUNCTION_NAME);
	if(fsName == NULL) {
		fsName = "FS";
	}
	int res = XLUtil::RegisterFunction(&xDLL, "FSExecute", "RCPPPPPPPPPP", fsName, 
		NULL, "1", "General", NULL, NULL, NULL, NULL);

	// Register execute function (volatile version (if requested))
	char* inclVol = iniparser_getstr(g_ini, FS_INCLUDE_VOLATILE);
	if(inclVol != NULL && strcmp(inclVol, "true") == 0) {
		char* fsvName = iniparser_getstr(g_ini, FS_FUNCTION_NAME_VOLATILE);
		if(fsvName == NULL) {
			fsvName = "FSV";
		}
		res = XLUtil::RegisterFunction(&xDLL, "FSExecuteVolatile", "RCPPPPPPPPPP!", fsvName, 
			NULL, "1", "General", NULL, NULL, NULL, NULL);
	}

	// Now ask server for a list of functions to register
	char* disableFunctionList = iniparser_getstr(g_ini, FS_DISABLE_FUNCTION_LIST);
	if(disableFunctionList == NULL || strcmp(disableFunctionList, "true")) {
		RegisterFunctions();
	}

	// Free the XLL filename
	Excel4(xlFree, 0, 1, (LPXLOPER) &xDLL);

	// OK
	return 1;
}

__declspec(dllexport) int WINAPI xlAutoClose(void)
{

	// Disconnect from server
	if(g_protocol != NULL) {
		g_protocol->disconnect();
		delete g_protocol;
		g_protocol = NULL;
	}

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
		xInfo.xltype = xltypeStr | xlbitXLFree;
		char* addinName = iniparser_getstr(g_ini, FS_ADDIN_NAME);
		if(addinName == NULL) {
			addinName = XLUtil::MakeExcelString("XLLoop v0.0.6");
		} else {
			addinName = XLUtil::MakeExcelString(addinName);
		}
		xInfo.val.str = addinName;
	} 

	return (LPXLOPER) &xInfo;
}

__declspec(dllexport) LPXLOPER WINAPI FSExecute(const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, LPXLOPER v4, 
												LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, LPXLOPER v9)
{
	// Attempt connection
	if(!InitProtocol()) {
		static XLOPER err;
		err.xltype = xltypeStr;
		err.val.str = " #Could not connect to server  ";
		return &err;
	}

	// Convert the args
	VTCollection* coll = new VTCollection;
	coll->add(XLConverter::ConvertX(v0));
	coll->add(XLConverter::ConvertX(v1));
	coll->add(XLConverter::ConvertX(v2));
	coll->add(XLConverter::ConvertX(v3));
	coll->add(XLConverter::ConvertX(v4));
	coll->add(XLConverter::ConvertX(v5));
	coll->add(XLConverter::ConvertX(v6));
	coll->add(XLConverter::ConvertX(v7));
	coll->add(XLConverter::ConvertX(v8));
	coll->add(XLConverter::ConvertX(v9));

	// Exec function
	Variant* res = g_protocol->executeFunction(name, coll);
	delete coll;

	// Check for error
	if(!g_protocol->isConnected()) {
		delete res;
		static XLOPER err;
		err.xltype = xltypeStr;
		err.val.str = " #Could not connect to server  ";
		return &err;
	}

	// Convert result
	LPXLOPER xres = XLConverter::ConvertV(res);
	delete res;

	return xres;
}

__declspec(dllexport) LPXLOPER WINAPI FSExecuteVolatile(const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, 
														LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, 
														LPXLOPER v9)
{
	// Just call off to main function (as this should have the same behaviour only volatile)
	return FSExecute(name, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9);
}

LPXLOPER WINAPI FSExecuteNumber(int number, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, 
								LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, 
								LPXLOPER v9)
{
	if(g_functionNames.size() < number) {
		static XLOPER err;
		err.xltype = xltypeStr; 
		err.val.str = "\020#Unknown function";
		return &err;
	}
	return FSExecute(g_functionNames[number].c_str(), v0, v1, v2, v3, v4, v5, v6, v7, v8, v9);
}

#define DECLARE_EXCEL_FUNCTION(number) \
__declspec(dllexport) LPXLOPER WINAPI FS##number (LPXLOPER v0, LPXLOPER v1, LPXLOPER v2 \
	,LPXLOPER v3, LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8 \
	,LPXLOPER v9) \
{ \
	return FSExecuteNumber(number, v0, v1, v2, v3, v4, v5, v6, v7, v8, v9); \
} \

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


#ifdef __cplusplus
}
#endif