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
#include <string>
#include "XLCodec.h"

class Protocol {
public:
	explicit Protocol(char* hostname, int port) : hostname(hostname), port(port), conn(0) {}
	virtual ~Protocol() { disconnect(); }

	int connect();
	void disconnect();
	bool isConnected() { return conn != NULL; }
	void setHost(char* hostname) {
		this->hostname = hostname;
	}
	void setPort(int port) {
		this->port = port;
	}
	LPXLOPER execute(const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, 
		LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, LPXLOPER v9) {
			send(name, v0, v1, v2, v3, v4,v5, v6, v7, v8, v9);
			return receive();
	}
	LPXLOPER execute(const char* name, int count, LPXLOPER v) {
		send(name, count, v);
		return receive();
	}
	LPXLOPER execute(const char* name) {
		send(name);
		return receive();
	}

private:
	int send(const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, 
		LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, LPXLOPER v9);
	int send(const char* name, int count, LPXLOPER v);
	int send(const char* name);
	LPXLOPER receive();

private:
	std::string hostname;
	int port;
	SOCKET conn;
};

#endif // PROTOCOL_H