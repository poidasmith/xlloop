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
#include "Protocol.h"
#include <Wininet.h>

class HttpProtocol : public Protocol {
public:
	HttpProtocol(const char* url);
	virtual ~HttpProtocol();

	virtual int connect() { return 0; }
	virtual void disconnect() {}
	virtual bool isConnected() { return true; }
	virtual void setHost(char* hostname) { }
	virtual void setPort(int port) { } 
	virtual LPXLOPER execute(const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, 
		LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, LPXLOPER v9);
	virtual LPXLOPER execute(const char* name);

private:
	LPXLOPER Execute(const char* name, LPXLOPER* args, int argc);

private:
	char* url;
	HINTERNET hSession;
};

#endif // HTTP_PROTOCOL_H