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
	res = XLUtil::RegisterFunction(&xDLL, "FSExecuteVolatile", "RRRRR!", "FSV", 
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
		xInfo.xltype = xltypeStr | xlbitXLFree;
		xInfo.val.str = XLUtil::MakeExcelString("Function Server v0.0.1");
	} 

	return (LPXLOPER) &xInfo;
}

Variant* ConvertX(LPXLOPER x);
Variant* ConvertXArray(LPXLOPER x)
{
	int rows = x->val.array.rows;
	int cols = x->val.array.columns;
	if(rows == 1 && cols == 1) {
		return ConvertX(&x->val.array.lparray[0]);
	}

	VTCollection* colr = new VTCollection;
	for(int i = 0; i < rows; i++) {
		VTCollection* colc = new VTCollection;
		for(int j = 0; j < cols; j++) {
			colc->add(ConvertX(&(x->val.array.lparray[i * cols + j])));
		}
		colr->add(colc);
	}

	return colr;
}

Variant* ConvertX(LPXLOPER x)
{
	if(x == NULL) return NULL;

	switch(x->xltype & ~(xlbitXLFree | xlbitDLLFree))
	{
	case xltypeBigData: 
		return NULL;
	case xltypeBool: 
		return new VTLong(x->val.boolean);
	case xltypeErr: 
		return NULL;
	case xltypeFlow: 
		return NULL;
	case xltypeInt: 
		return new VTLong(x->val.w);
	case xltypeMissing: 
		return NULL;
	case xltypeMulti: 
		return ConvertXArray(x);
	case xltypeNil: 
		return NULL;
	case xltypeNum: 
		return new VTDouble(x->val.num);
	case xltypeRef: 
	case xltypeSRef: 
		{
			LPXLOPER xMulti = (LPXLOPER) malloc(sizeof(XLOPER));
			XLOPER xTempMulti;
			xTempMulti.xltype = xltypeInt;
			xTempMulti.val.w = xltypeMulti;

			if ( xlretUncalced ==
					Excel4( xlCoerce, (LPXLOPER) xMulti, 2, (LPXLOPER) x,
								(LPXLOPER) &xTempMulti ) )
			{
				free(xMulti);
				return NULL;
			}

			Variant* v = ConvertXArray(xMulti);
			free(x);
			return v;
		}
	case xltypeStr: 
		{
			char chars[MAX_PATH];
			int len = x->val.str[0];
			memcpy(chars, &x->val.str[1], len);
			chars[len] = 0;
			return new VTString(chars);
		}
	}

	return NULL;
}

LPXLOPER ConvertV(const Variant* v)
{
	if(v == NULL) return NULL;
	LPXLOPER xl = (LPXLOPER) malloc(sizeof(XLOPER));
	xl->xltype = xltypeStr | xlbitXLFree;
	xl->val.str = " #Unknown Error";

	switch(v->getType()) {
	case VSTRUCT:
		{
			VTStruct* struc = (VTStruct*) v;
			int rows = struc->size();
			xl->val.array.rows = rows;
			xl->val.array.columns = 2;
			xl->val.array.lparray = (LPXLOPER) malloc(sizeof(XLOPER) * rows * 2);
			for(int i = 0; i < rows; i++) {
				const char* key = struc->getKey(i);
				const Variant* vi = struc->getValue(i);
				xl->val.array.lparray[rows * i].xltype = xltypeStr;
				xl->val.array.lparray[rows * i].val.str = XLUtil::MakeExcelString(key);
				LPXLOPER pxl = ConvertV(vi);
				memcpy(&(xl->val.array.lparray[2 * i + 1]), pxl, sizeof(XLOPER));
				free(pxl);
			}
			xl->xltype = xltypeMulti | xlbitXLFree;
		}
		break;
	case VCOLLECTION:
		{
			VTCollection* coll = (VTCollection*) v;
			int rows = coll->size();
			Variant* v0 = coll->get(0);
			if(v0->getType() != VCOLLECTION) {
				return NULL;
			}
			int cols = ((VTCollection*)v0)->size();
			xl->val.array.rows = rows;
			xl->val.array.columns = cols;
			xl->val.array.lparray = (LPXLOPER) malloc(sizeof(XLOPER) * rows * cols);
			for(int i = 0; i < rows; i++) {
				Variant* vi = coll->get(i);
				if(vi == NULL || v->getType() != VCOLLECTION) {
					free(xl->val.array.lparray);
					free(xl);
					return NULL;
				}
				VTCollection* vci = (VTCollection*) vi;
				int csize = vci->size();
				for(int j = 0; j < csize; j++) {
					LPXLOPER pxl = ConvertV(vci->get(j));
					memcpy(&(xl->val.array.lparray[rows * i + j]), pxl, sizeof(XLOPER));
					free(pxl);
				}
			}
			xl->xltype = xltypeMulti | xlbitXLFree;
		}
		break;
	case VSTRING:
		xl->xltype = xltypeStr | xlbitXLFree;
		xl->val.str = XLUtil::MakeExcelString(((VTString*)v)->get());
		break;
	case VDOUBLE:
		xl->xltype = xltypeNum | xlbitXLFree;
		xl->val.num = ((VTDouble*)v)->get();
		break;
	case VLONG:
		xl->xltype = xltypeNum | xlbitXLFree;
		xl->val.num = ((VTLong*)v)->get();
		break;
	}

	return xl;
}

__declspec(dllexport) LPXLOPER WINAPI FSExecute(LPXLOPER name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2)
{
	if(g_protocol == NULL) {
		g_protocol = new Protocol("localhost", 5454);
	}

	if(g_protocol->connect()) {
		static XLOPER err;
		err.xltype = xltypeStr;
		err.val.str = " #Could not connect to server  ";
		return &err;
	}

	VTStruct* args = new VTStruct;
	args->add("name", ConvertX(name));
	VTCollection* coll = new VTCollection;
	Variant* vv0 = ConvertX(v0); if(vv0) coll->add(vv0);
	Variant* vv1 = ConvertX(v1); if(vv1) coll->add(vv1);
	Variant* vv2 = ConvertX(v2); if(vv2) coll->add(vv2);
	args->add("args", coll);
	Variant* res = g_protocol->execute("Exec", args);
	delete args;
	if(!g_protocol->isConnected()) {
		delete res;
		static XLOPER err;
		err.xltype = xltypeStr;
		err.val.str = " #Could not connect to server  ";
		return &err;
	}
	LPXLOPER xres = ConvertV(res);
	delete res;

	return xres;
}

__declspec(dllexport) LPXLOPER WINAPI FSExecuteVolatile(LPXLOPER name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2)
{
	return FSExecute(name, v0, v1, v2);
}

#ifdef __cplusplus
}
#endif