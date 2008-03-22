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
#include "functionserver/Protocol.h"
#include "functionserver/XLConverter.h"

// The DLL instance
static HINSTANCE g_hinstance = NULL;

// The INI file
static dictionary* g_ini = NULL;

// The protocol manager
static Protocol* g_protocol = NULL;

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

	return 1;
}

#ifdef __cplusplus
extern "C" {  
#endif 

__declspec(dllexport) int WINAPI xlAutoOpen(void)
{
	static XLOPER xDLL;
	Excel4(xlGetName, &xDLL, 0);

	// Register execute function
	int res = XLUtil::RegisterFunction(&xDLL, "FSExecute", "RCPPP", "FS", 
		NULL, "1", "General", NULL, NULL, NULL, NULL);

	// Register execute function (volatile version)
	res = XLUtil::RegisterFunction(&xDLL, "FSExecuteVolatile", "RCPPP!", "FSV", 
		NULL, "1", "General", NULL, NULL, NULL, NULL);

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

	if(xIntAction.val.w == 1) {
		xInfo.xltype = xltypeStr | xlbitXLFree;
		xInfo.val.str = XLUtil::MakeExcelString("Function Server v0.0.1");
	} 

	return (LPXLOPER) &xInfo;
}

__declspec(dllexport) LPXLOPER WINAPI FSExecute(char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2)
{
	// Create our protocol manager
	if(g_protocol == NULL) {
		g_protocol = new Protocol("localhost", 5454);
	}

	// Attempt connection
	if(g_protocol->connect()) {
		static XLOPER err;
		err.xltype = xltypeStr;
		err.val.str = " #Could not connect to server  ";
		return &err;
	}

	// Convert the args
	VTCollection* coll = new VTCollection;
	while(true) {
		Variant* vv0 = XLConverter::ConvertX(v0, false); if(vv0) coll->add(vv0); else break;
		Variant* vv1 = XLConverter::ConvertX(v1, false); if(vv1) coll->add(vv1); else break;
		Variant* vv2 = XLConverter::ConvertX(v2, false); if(vv2) coll->add(vv2); else break;
		break;
	}

	// Exec function
	Variant* res = g_protocol->execFunction(name, coll);

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

__declspec(dllexport) LPXLOPER WINAPI FSExecuteVolatile(char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2)
{
	return FSExecute(name, v0, v1, v2);
}

#ifdef __cplusplus
}
#endif