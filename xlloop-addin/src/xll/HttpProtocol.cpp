/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "HttpProtocol.h"
#include "JSONCodec.h"
#include "../common/Log.h"
#include "XLUtil.h"

#define USER_AGENT L"XLLoop/Http v0.1.0"

#define REQ_TYPE_NAME "request"
#define REQ_TYPE_VAL "XLLoop"
#define REQ_VER_NAME "version"
#define REQ_VER_VAL "0.1.0"
#define REQ_NAME_NAME "name"
#define REQ_ARGS_NAME "args"

typedef struct _ctx {
	HINTERNET hConnect;
	HINTERNET hRequest;
	HANDLE hEvent;
	yajl_gen_config conf;
	yajl_gen g;
	LPXLOPER px;
} REQUEST_CONTEXT;

VOID CALLBACK CallBack(HINTERNET session, DWORD_PTR context, DWORD status, LPVOID statusInfo, DWORD statusInfoLen);

void GenerateRequest(yajl_gen g, const char* fn, LPXLOPER* argv, int argc)
{
	yajl_gen_map_open(g);
	yajl_gen_string(g, (const unsigned char*) REQ_TYPE_NAME, strlen(REQ_TYPE_NAME));
	yajl_gen_string(g, (const unsigned char*) REQ_TYPE_VAL, strlen(REQ_TYPE_VAL));
	yajl_gen_string(g, (const unsigned char*) REQ_VER_NAME, strlen(REQ_VER_NAME));
	yajl_gen_string(g, (const unsigned char*) REQ_VER_VAL, strlen(REQ_VER_VAL));
	yajl_gen_string(g, (const unsigned char*) REQ_NAME_NAME, strlen(REQ_NAME_NAME));
	yajl_gen_string(g, (const unsigned char*) fn, strlen(fn));
	yajl_gen_string(g, (const unsigned char*) REQ_ARGS_NAME, strlen(REQ_ARGS_NAME));
	yajl_gen_array_open(g);
	for(int i = 0; i < argc; i++) {
		JSONCodec::Encode(g, argv[i]);
	}
	yajl_gen_array_close(g);
	yajl_gen_map_close(g);
}

HttpProtocol::HttpProtocol(const char* url)
{
	Log::Info("Setup session for: %s", url);
	int usz = MultiByteToWideChar(CP_ACP, 0, url, strlen(url), 0, 0);
	this->url = (wchar_t*) malloc((usz + 1) * sizeof(wchar_t));
	MultiByteToWideChar(CP_ACP, 0, url, strlen(url), this->url, usz);
	this->url[usz] = 0;
	memset(&urlc, 0, sizeof(URL_COMPONENTS));
	urlc.dwStructSize = sizeof(URL_COMPONENTS);
	urlc.dwSchemeLength = 1;
	urlc.dwHostNameLength = 1;
	urlc.dwUserNameLength = 1;
	urlc.dwPasswordLength = 1;
	urlc.dwUrlPathLength = 1;
	urlc.dwExtraInfoLength = 1;
	WinHttpCrackUrl(this->url, usz, 0, &urlc);
	this->host = (wchar_t*) malloc((urlc.dwHostNameLength + 1) * sizeof(wchar_t));
	memcpy(this->host, urlc.lpszHostName, urlc.dwHostNameLength*sizeof(wchar_t));
	this->host[urlc.dwHostNameLength] = 0;
	this->path = (wchar_t*) malloc((urlc.dwUrlPathLength + 1) * sizeof(wchar_t));
	memcpy(this->path, urlc.lpszUrlPath, urlc.dwUrlPathLength*sizeof(wchar_t));
	this->path[urlc.dwUrlPathLength] = 0;
	WINHTTP_CURRENT_USER_IE_PROXY_CONFIG proxy;
	WinHttpGetIEProxyConfigForCurrentUser(&proxy);
	int proxyType = WINHTTP_ACCESS_TYPE_NO_PROXY;
	if(proxy.lpszProxy)
		proxyType = WINHTTP_ACCESS_TYPE_NAMED_PROXY;
	hSession = WinHttpOpen(USER_AGENT, proxyType,
		proxy.lpszProxy, proxy.lpszProxyBypass, 0);
}

HttpProtocol::~HttpProtocol()
{
	WinHttpCloseHandle(hSession);
	if(url) free(url);
	if(host) free(host);
	if(path) free(path);
}

LPXLOPER HttpProtocol::execute(const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, 
	LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, LPXLOPER v9)
{
	LPXLOPER args[10];
	args[0] = v0;
	args[1] = v1;
	args[2] = v2;
	args[3] = v3;
	args[4] = v4;
	args[5] = v5;
	args[6] = v6;
	args[7] = v7;
	args[8] = v8;
	args[9] = v9;
	return Execute(name, args, 10);
}

LPXLOPER HttpProtocol::execute(const char* name)
{
	return Execute(name, 0, 0);
}

#define BUFFER_SIZE 8192

VOID ReadData(REQUEST_CONTEXT* context)
{
	//Log::Info("Reading data... %d", context->hRequest);
	yajl_handle hand;
	yajl_parser_config cfg = { 1, 1 };
	json_ctx ctx = { 0, 0 };
	hand = JSONCodec::AllocateHandle(&cfg, &ctx);

	unsigned char temp[BUFFER_SIZE];
	DWORD read = 0;
	BOOL res = FALSE;
	while(true) {
		res = WinHttpReadData(context->hRequest, temp, BUFFER_SIZE, &read);
		if(read == 0)
			break;
		temp[read] = 0;
		yajl_parse(hand, temp, read);
	}

	yajl_parse_complete(hand);
    yajl_free(hand);
	JSONCodec::Decode(ctx.current, context->px);
	JSONCodec::FreeJsonValue(ctx.current);
}

LPXLOPER HttpProtocol::Execute(const char* name, LPXLOPER* args, int argc)
{
	//Log::Info("Execute: %s", name);
	REQUEST_CONTEXT context;
	context.hEvent = CreateEvent(0, 1, 0, 0);
	context.hConnect = WinHttpConnect(hSession, host, urlc.nPort, 0);
	int flags = WINHTTP_FLAG_BYPASS_PROXY_CACHE;
	if(urlc.nScheme == INTERNET_SCHEME_HTTPS)
		flags |= WINHTTP_FLAG_SECURE;
	context.hRequest = WinHttpOpenRequest(context.hConnect, L"POST", path, 0, 0, 0, 
		flags);
	context.conf.beautify = 0;
	context.conf.indentString = "";
	context.g = yajl_gen_alloc(&context.conf, 0);
	context.px = (LPXLOPER) malloc(sizeof(XLOPER));
	context.px->xltype = xltypeNil | xlbitDLLFree;
	GenerateRequest(context.g, name, args, argc);
	const unsigned char * buf;
    unsigned int len = 0;
    yajl_gen_get_buf(context.g, &buf, &len);
	BOOL res = FALSE;
	res = WinHttpSendRequest(context.hRequest, 0, 0, (LPVOID) buf, len, len, (DWORD_PTR) &context);
	if(!res) {
		const char* err = "#Could not connect to server";
		Log::Error(err);
		WinHttpCloseHandle(context.hRequest);
		WinHttpCloseHandle(context.hConnect);
		context.px->xltype = xltypeStr;
		context.px->val.str = XLUtil::MakeExcelString(err);
		return context.px;
	}
	// TODO timeout/background
	res = WinHttpReceiveResponse(context.hRequest, 0);
	ReadData(&context);
	WinHttpCloseHandle(context.hRequest);
	WinHttpCloseHandle(context.hConnect);
    yajl_gen_clear(context.g);
	yajl_gen_free(context.g);
	context.px->xltype | xlbitDLLFree;
	return context.px;
}


