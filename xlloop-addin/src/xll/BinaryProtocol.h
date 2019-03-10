/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef PROTOCOL_H
#define PROTOCOL_H

#include "windows.h"
#include "XLCodec.h"
#include "../Common/Dictionary.h"

#define SERVER_SELECT_MODE_RANDOM 0
#define SERVER_SELECT_MODE_ROUND_ROBIN 1

class Protocol {
public:
	virtual ~Protocol() {}
	virtual void initialize(dictionary* ini, const XCHAR* section) = 0;
	virtual int connect() = 0;
	virtual void disconnect() = 0;
	virtual bool isConnected() = 0;
	virtual LPXLOPER12 getLastError() = 0;
	virtual LPXLOPER12 execute(const XCHAR* name, bool sendCaller, int count, ...) = 0;
	virtual LPXLOPER12 execute(const XCHAR* name, bool sendCaller, int count, LPXLOPER12 far opers[]) = 0;
};

class BinaryProtocol : public Protocol {
public:
	BinaryProtocol() : servers(0), serverPorts(0), serverCount(0), conn(0), is(conn), os(conn) {}
	virtual ~BinaryProtocol() { disconnect(); }
	void initialize(dictionary* ini, const WCHAR* section);
	int connect();
	void disconnect();
	bool isConnected() { return conn != NULL; }
	LPXLOPER12 getLastError() { return &errorMessage; }
	LPXLOPER12 execute(const XCHAR* name, bool sendCaller, int count, ...);
	LPXLOPER12 execute(const XCHAR* name, bool sendCaller, int count, LPXLOPER12 far opers[]);

private:
	int connect(int selectionMode);
	int connect(XCHAR* hostname, int port);
	int send(const XCHAR* name, bool sendSource, int count, LPXLOPER12 far opers[]);
	LPXLOPER12 receive(const XCHAR* name);
	void ParseServerList(XCHAR* server);
	int ExtractPort(XCHAR* server);

private:
	// The list of available servers
	WCHAR** servers;
	int* serverPorts;
	int serverCount;
	int selectedServer;
	int selectionMode;
	int retryCount;
	SOCKET conn;
	XIStream is;
	XOStream os;
	XLOPER12 errorMessage;
};

#endif // PROTOCOL_H