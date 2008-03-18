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

// The DLL instance
static HINSTANCE g_hinstance = NULL;

// The INI file
static dictionary* g_ini = NULL;

// The protocol manager
static Protocol* g_protocol = NULL;

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved)
{
	if(fdwReason == DLL_PROCESS_ATTACH)
	{
		g_hinstance = hinstDLL;
		g_ini = INI::LoadIniFile(hinstDLL);
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
	int res = XLUtil::RegisterFunction(&xDLL, "FSExecute", "RRRRR", "FS", 
		NULL, "1", "General", NULL, NULL, NULL, NULL);

	// Register execute function (volatile version)
	res = XLUtil::RegisterFunction(&xDLL, "FSExecute", "RRRRR!", "FSV", 
		NULL, "1", "General", NULL, NULL, NULL, NULL);

	// Free the XLL filename
	Excel4(xlFree, 0, 1, (LPXLOPER) &xDLL);

	// OK
	return 1;
}

__declspec(dllexport) int WINAPI xlAutoClose(void)
{
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

__declspec(dllexport) LPXLOPER WINAPI xlAddInManagerInfo(LPXLOPER xAction)
{
	static XLOPER xInfo, xIntAction, xIntType;
	xIntType.xltype = xltypeInt;
	xIntType.val.w = xltypeInt;
	xInfo.xltype = xltypeErr;
	xInfo.val.err = xlerrValue;

	Excel4(xlCoerce, &xIntAction, 2, xAction, &xIntType);

	if(xIntAction.val.w == 1) {
		xInfo.xltype = xltypeStr;
		xInfo.val.str = XLUtil::MakeExcelString("Function Server v0.0.1");
	} 

	return (LPXLOPER) &xInfo;
}

Variant* ConvertX(LPXLOPER x)
{
	if(x == NULL) return NULL;

	switch(x->xltype & ~(xlbitXLFree | xlbitDLLFree))
	{
	case xltypeBigData: 
		return NULL;
	case xltypeBool: 
		return NULL;
	case xltypeErr: 
		return NULL;
	case xltypeFlow: 
		return NULL;
	case xltypeInt: 
		return NULL;
	case xltypeMissing: 
		return NULL;
	case xltypeMulti: 
		return NULL;
	case xltypeNil: 
		return NULL;
	case xltypeNum: 
		return NULL;
	case xltypeRef: 
		return NULL;
	case xltypeSRef: 
		return NULL;
	case xltypeStr: 
		return new VTString(x->val.str);
	}

	return NULL;
}

LPXLOPER ConvertV(Variant* v)
{
	if(v == NULL) return NULL;
	LPXLOPER xl = new XLOPER;
	xl->xltype = xltypeErr | xlbitXLFree;
	xl->val.str = "#Unknown Error";


	switch(v->getType()) {
	case VSTRUCT:
		break;
	case VCOLLECTION:
		break;
	case VSTRING:
		xl->xltype = xltypeStr | xlbitXLFree;
		xl->val.str = _strdup(((VTString*)v)->get());
		break;
	case VDOUBLE:
		break;
	case VLONG:
		break;
	}

	return xl;
}

__declspec(dllexport) LPXLOPER WINAPI FSExecute(LPXLOPER name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2)
{
	if(g_protocol == NULL) {
		g_protocol = new Protocol("localhost", 5454);
		g_protocol->connect();
	}

	VTStruct* args = new VTStruct;
	args->add("name", ConvertX(name));
	VTCollection* coll = new VTCollection;
	coll->add(ConvertX(v0));
	coll->add(ConvertX(v1));
	coll->add(ConvertX(v2));
	args->add("args", coll);
	Variant* res = g_protocol->execute("Exec", args);
	LPXLOPER xres = ConvertV(res);
	delete args;
	delete res;
	return xres;
}

#ifdef __cplusplus
}
#endif