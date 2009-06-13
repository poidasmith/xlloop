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

#define USER_AGENT "XLLoop/Http v0.1.0"

#define REQ_TYPE_NAME "request"
#define REQ_TYPE_VAL "XLLoop"
#define REQ_VER_NAME "version"
#define REQ_VER_VAL "0.1.0"
#define REQ_NAME_NAME "name"
#define REQ_ARGS_NAME "args"

VOID CALLBACK CallBack(HINTERNET session, DWORD_PTR context, DWORD status, LPVOID statusInfo, DWORD statusInfoLen);

void SendRequest(HINTERNET hRequest, yajl_gen g, const char* fn, LPXLOPER* argv, int argc)
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
	const unsigned char * buf;
    unsigned int len = 0;
    yajl_gen_get_buf(g, &buf, &len);
	HttpSendRequest(hRequest, 0, 0, (LPVOID) buf, len);
}

HttpProtocol::HttpProtocol(const char* url)
{
	Log::Info("Setup session for: %s", url);
	this->url = strdup(url);
	memset(&urlc, 0, sizeof(URL_COMPONENTS));
	urlc.dwStructSize = sizeof(URL_COMPONENTS);
	urlc.dwSchemeLength = 1;
	urlc.dwHostNameLength = 1;
	urlc.dwUserNameLength = 1;
	urlc.dwPasswordLength = 1;
	urlc.dwUrlPathLength = 1;
	urlc.dwExtraInfoLength = 1;
	InternetCrackUrl(this->url, strlen(this->url), 0, &urlc);
	hSession = InternetOpen(USER_AGENT, INTERNET_OPEN_TYPE_PRECONFIG,
		NULL, NULL, INTERNET_FLAG_ASYNC);
	INTERNET_STATUS_CALLBACK callback = InternetSetStatusCallback(hSession, (INTERNET_STATUS_CALLBACK) CallBack);
}

HttpProtocol::~HttpProtocol()
{
	InternetSetStatusCallback(hSession, NULL);
	InternetCloseHandle(hSession);
	if(url) free(url);
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

typedef struct _ctx {
	HINTERNET hConnect;
	HINTERNET hRequest;
	HANDLE hEvent;
	yajl_gen_config conf;
	yajl_gen g;
	LPXLOPER px;
} REQUEST_CONTEXT;

VOID ReadData(REQUEST_CONTEXT* context)
{
	//Log::Info("Reading data... %d", context->hRequest);
	yajl_handle hand;
	yajl_parser_config cfg = { 1, 1 };
	json_ctx ctx = { 0, 0 };
	hand = JSONCodec::AllocateHandle(&cfg, &ctx);

	unsigned char temp[MAX_PATH + 1];
	memset(temp, 0, MAX_PATH+1);
	DWORD read = 0;
	BOOL res = FALSE;
	BOOL goterr = FALSE;
	while(true) {
		res = InternetReadFile(context->hRequest, temp, MAX_PATH, &read);
		if(!res) {
			DWORD err = GetLastError();
			//Log::Info("Error in buggy wininet: %d", err);
			if(err == 997) {
				goterr = TRUE;
				int l = strlen((const char *) temp);
				if(l != 0) {
					yajl_parse(hand, temp, l);
				} else {
					Sleep(10);
				}
				continue;
			} else {
				break;
			}
		}
		if(read == 0)
			break;
		temp[read] = 0;
		yajl_parse(hand, temp, read);
	}

	if(res && goterr) {
		Log::Info("Recovered...");
	}

	if(!res) {
		DWORD err = GetLastError();
		DWORD err2;
		char errStr[MAX_PATH];
		memset(errStr, 0, MAX_PATH);
		DWORD sz = MAX_PATH;
		InternetGetLastResponseInfo(&err2, errStr, &sz);
		Log::Info("Error in protocol: %d %d %s", err, err2, errStr);
	}

	yajl_parse_complete(hand);
    yajl_free(hand);

	res = JSONCodec::Decode(ctx.current, context->px);
	JSONCodec::FreeJsonValue(ctx.current);
}

LPXLOPER HttpProtocol::Execute(const char* name, LPXLOPER* args, int argc)
{
	//Log::Info("Execute: %s", name);
	REQUEST_CONTEXT context;
	char host[MAX_PATH];
	char path[MAX_PATH];
	memcpy(host, urlc.lpszHostName, urlc.dwHostNameLength);
	host[urlc.dwHostNameLength] = 0;
	memcpy(path, urlc.lpszUrlPath, urlc.dwUrlPathLength);
	path[urlc.dwUrlPathLength] = 0;
	context.hEvent = CreateEvent(0, 1, 0, 0);
	context.hConnect = InternetConnect(hSession, host, urlc.nPort, 0, 0, 
		INTERNET_SERVICE_HTTP, 0, (DWORD_PTR) this);
	context.hRequest = HttpOpenRequest(context.hConnect, "POST", path, 0, 0, 0, 
		INTERNET_FLAG_RELOAD | INTERNET_FLAG_NO_CACHE_WRITE, (DWORD_PTR) &context);
	context.conf.beautify = 0;
	context.conf.indentString = "";
	context.g = yajl_gen_alloc(&context.conf, 0);
	context.px = (LPXLOPER) malloc(sizeof(XLOPER));
	SendRequest(context.hRequest, context.g, name, args, argc);
	WaitForSingleObject(context.hEvent, INFINITE); // TODO add timeout spinner
	CloseHandle(context.hEvent);
	ReadData(&context);
	InternetCloseHandle(context.hRequest);
	InternetCloseHandle(context.hConnect);
    yajl_gen_clear(context.g);
	yajl_gen_free(context.g);
	context.px->xltype | xlbitDLLFree;
	return context.px;
}

VOID CALLBACK CallBack(HINTERNET session, DWORD_PTR context, DWORD status, LPVOID statusInfo, DWORD statusInfoLen)
{
	//Log::Info("callback: %d", status);
	switch(status) {
	case INTERNET_STATUS_COOKIE_SENT:
		//printf("Cookie sent\n");
		break;
	case INTERNET_STATUS_COOKIE_RECEIVED:
		//printf("Cookie received\n");
		break;
    case INTERNET_STATUS_CLOSING_CONNECTION:
        //printf("Closing Connection\n");
        break;
    case INTERNET_STATUS_CONNECTED_TO_SERVER:
        //printf("Connected to Server\n");
        break;
    case INTERNET_STATUS_CONNECTING_TO_SERVER:
        //printf("Connecting to Server\n");
        break;
    case INTERNET_STATUS_CONNECTION_CLOSED:
        //printf("Connection Closed\n");
        break;
    case INTERNET_STATUS_HANDLE_CREATED:
        //printf("Handle Created\n");
        break;
    case INTERNET_STATUS_HANDLE_CLOSING:
        //printf("Handle Closing\n");
        break;
    case INTERNET_STATUS_INTERMEDIATE_RESPONSE:
        //printf("Intermediate response\n");
        break;
    case INTERNET_STATUS_RECEIVING_RESPONSE:
        //printf("Receiving Response\n");    
        break;
    case INTERNET_STATUS_RESPONSE_RECEIVED:
        //printf("Response Received (%d bytes)\n", *((LPDWORD)statusInfo));   
	    break;
    case INTERNET_STATUS_REQUEST_SENT:
        //printf("Request Sent\n");    
	    break;
    case INTERNET_STATUS_REQUEST_COMPLETE:
        //printf("Request Complete\n");    
		SetEvent(((REQUEST_CONTEXT*)context)->hEvent);
	    break;
	case INTERNET_STATUS_DETECTING_PROXY:
        //printf("Detecting Proxy\n");
        break;            
    case INTERNET_STATUS_RESOLVING_NAME:
        //printf("Resolving Name\n");
        break;
    case INTERNET_STATUS_NAME_RESOLVED:
        //printf("Name Resolved\n");
        break;
    case INTERNET_STATUS_SENDING_REQUEST:
        //printf("Sending request\n");
        break;
    case INTERNET_STATUS_STATE_CHANGE:
        //printf("State Change\n");
        break;
    case INTERNET_STATUS_P3P_HEADER:
        //printf("Received P3P header\n");
        break;
    default:
        //printf("Unknown (%d)\n", status);
        break;	
	}
}


