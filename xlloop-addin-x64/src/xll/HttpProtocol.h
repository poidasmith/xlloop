/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef HTTP_PROTOCOL_H
#define HTTP_PROTOCOL_H

#include "windows.h"
#include "BinaryProtocol.h"
#include <Winhttp.h>

class HttpProtocol : public Protocol {
public:
	HttpProtocol() : hasConnected(false) {}
	virtual ~HttpProtocol();
	virtual void initialize(dictionary *ini, const char* section);
	virtual int connect() { return 0; }
	virtual void disconnect() {}
	virtual bool isConnected() { return hasConnected ? true : !(hasConnected=true); }
	LPXLOPER getLastError() { return &errorMessage; }
	LPXLOPER execute(const char* name, bool sendCaller, int count, ...);
	LPXLOPER execute(const char* name, bool sendCaller, int count, LPXLOPER far opers[]);

private:
	LPXLOPER Execute(const char* name, bool sendCaller, LPXLOPER* args, int argc);

private:
	wchar_t* url;
	wchar_t* host;
	wchar_t* path;
	HINTERNET hSession;
	URL_COMPONENTS urlc;
	bool hasConnected;
	XLOPER errorMessage;
};

#endif // HTTP_PROTOCOL_H