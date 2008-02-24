/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "../common/Runtime.h"
#include "../common/COMHelper.h"
#include "../common/INI.h"
#include "../common/Log.h"
#include "../java/VM.h"
#include "../java/JNI.h"
#include "../java/Classpath.h"
#include "../xll/xlcall.h"
#include "../xll/XLUtil.h"
#include "../xll/XLObject.h"
#include "../xll/XLObjectFactory.h"
#include "../xll/XLAddin.h"
#include "../../build/XLLServer_h.h"
#include "../../build/XLLServer_i.c"
#include <stdio.h>
#include <jni.h>

#define MAX_ARGS 30
#define ADDIN_CLASS ":addin.class"

extern "C" int __cdecl _purecall()
{
	return 0;
}

typedef jint (JNICALL *JNI_createJavaVM)(JavaVM **pvm, JNIEnv **env, void *args);

#define E_VM_NOT_CREATED 1
#define E_LIBRARY_NOT_FOUND 2
#define E_CREATE_FUNCTION_NOT_FOUND 3;

#define STDMETHOD HRESULT __stdcall

class XLLServer : public COMBase<IXLLServer> {
public:
	XLLServer() : callback(0) {}
	STDMETHOD Startup(IXLLCallback* callback, int* result);
	STDMETHOD GetFunctionCount(int* count);
	STDMETHOD GetFunction(int index, SAFEARRAY* info);
	STDMETHOD GetCommandCount(int* count);
	STDMETHOD GetCommand(int index, SAFEARRAY* info);
	STDMETHOD ExecuteFunction(BSTR name, SAFEARRAY* args, wireXLOPER* result);
	STDMETHOD ExecuteCommand(BSTR name);
	STDMETHOD Shutdown(int* result);

private:
	BOOL StartupVM();
	BOOL Initialize();

private:
	BOOL initialized;
	dictionary* ini;
	XLAddin* addin;
	IXLLCallback* callback;
};

int RegisterServer()
{
	return 0;
}

int UnregisterServer()
{
	return 0;
}

ClassFactoryBase<XLLServer> factory;

int __stdcall WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) 
{
	DebugBreak();
	if (strcmp (lpCmdLine, "Register") == 0) {
		return RegisterServer();
	}
	else if (strstr (lpCmdLine, "Unregister")) {
		return UnregisterServer();
	}

	CoInitialize(0);	
	DWORD registerId = 0;
	HRESULT hr = CoRegisterClassObject(CLSID_XLLServer, &factory, CLSCTX_SERVER, REGCLS_SINGLEUSE, &registerId);
	if(FAILED(hr))
		return hr;

	MSG msg;
	while (GetMessage(&msg, 0, 0, 0) > 0) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	CoRevokeClassObject(registerId);
	CoUninitialize();

	return 0;
}


STDMETHOD XLLServer::Startup(IXLLCallback* callback, int* result)
{
	if(callback == NULL || result == 0) {
		return E_INVALIDARG;
	}

	// Attempt to find an appropriate java VM
	char* vmlibrary = VM::FindJavaVMLibrary(ini);


	return E_NOTIMPL;
}

STDMETHOD XLLServer::GetFunctionCount(int* count)
{
	return E_NOTIMPL;
}

STDMETHOD XLLServer::GetFunction(int index, SAFEARRAY* info)
{
	return E_NOTIMPL;
}

STDMETHOD XLLServer::GetCommandCount(int* count)
{
	return E_NOTIMPL;
}

STDMETHOD XLLServer::GetCommand(int index, SAFEARRAY* info)
{
	return E_NOTIMPL;
}

STDMETHOD XLLServer::ExecuteFunction(BSTR name, SAFEARRAY* args, wireXLOPER* result)
{
	return E_NOTIMPL;
}

STDMETHOD XLLServer::ExecuteCommand(BSTR name)
{
	return E_NOTIMPL;
}

STDMETHOD XLLServer::Shutdown(int* result)
{
	return E_NOTIMPL;
}

BOOL XLLServer::StartupVM()
{
	// Attempt to load the ini file for this module
	ini = INI::LoadIniFile(NULL);

	if(ini == NULL) {
		Log::SetLastError("Could not find module ini file");
		return false;
	}

	// Attempt to find an appropriate java VM
	char* vmlibrary = VM::FindJavaVMLibrary(ini);
	if(!vmlibrary) {
		Log::SetLastError("Could not find VM");
		return false;
	}

	// Collect the VM args from the INI file
	TCHAR *vmargs[MAX_PATH];
	int vmargsCount = 0;
	INI::GetNumberedKeysFromIni(ini, VM_ARG, vmargs, vmargsCount);

	// Build up the classpath and add to vm args
	Classpath::BuildClassPath(ini, vmargs, vmargsCount);

	// Extract the specific VM args
	VM::ExtractSpecificVMArgs(ini, vmargs, vmargsCount);

	// Make sure there is a NULL at the end of the args
	vmargs[vmargsCount] = NULL;

	// Fire up the VM
	int vmerr = VM::StartJavaVM(vmlibrary, vmargs, NULL);
	if(vmerr != 0) {
		Log::SetLastError("VM could not be started (returned %d)", vmerr);
		return false;
	}

	// Free vm args
	for(int i = 0; i < vmargsCount; i++) {
		free(vmargs[i]);
	}

	return true;
}

BOOL XLLServer::Initialize()
{
	if(initialized) return true;

	bool result = StartupVM();
	if(!result) {
		return result;
	}

	JNIEnv* env = VM::GetJNIEnv();
	result = INI::RegisterNatives(env, true);
	if(!result) {
		return result;
	}

	result = XLUtil::RegisterNatives(env, "TODO");
	if(!result) {
		return result;
	}

	result = XLObjectFactory::RegisterNatives(env);
	if(!result) {
		return result;
	}

	result = XLObject::RegisterNatives(env);
	if(!result) {
		return result;
	}

	addin = new XLAddin;
	result = addin->Load(env, iniparser_getstr(ini, ADDIN_CLASS));
	if(!result) {
		return result;
	}
	
	return initialized = true;
}
