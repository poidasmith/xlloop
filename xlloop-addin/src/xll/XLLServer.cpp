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
#include "../../build/XLLServer_h.h"
#include "../../build/XLLServer_i.c"
#include <stdio.h>
#include <jni.h>

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
	XLLServer() : callback(0), jvm(0), env(0), hModule(0) {}
	STDMETHOD Startup(IXLLCallback* callback, int* result);
	STDMETHOD GetFunctionCount(int* count);
	STDMETHOD GetFunction(int index, SAFEARRAY* info);
	STDMETHOD GetCommandCount(int* count);
	STDMETHOD GetCommand(int index, SAFEARRAY* info);
	STDMETHOD ExecuteFunction(BSTR name, SAFEARRAY* args, wireXLOPER* result);
	STDMETHOD ExecuteCommand(BSTR name);
	STDMETHOD Shutdown(int* result);

private:
	IXLLCallback* callback;
	JavaVM* jvm;
	JNIEnv* env;
	HMODULE hModule;
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
