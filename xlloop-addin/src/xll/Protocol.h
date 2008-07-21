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
#include "../common/VTCodec.h"

#define REQ_TYPE_GENERIC 0
#define REQ_TYPE_FUNCTION 1

class Protocol {
public:
	explicit Protocol(char* hostname, int port) : hostname(hostname), port(port), conn(0) {}
	virtual ~Protocol() { disconnect(); }

	int connect();
	void disconnect();
	int send(unsigned char type, const char* name, Variant* args);
	Variant* receive();
	Variant* executeGeneric(const char* name, Variant* args) {
		send(REQ_TYPE_GENERIC, name, args);
		return receive();
	}
	Variant* executeFunction(const char* name, VTCollection* args) {
		send(REQ_TYPE_FUNCTION, name, args);
		return receive();
	}
	bool hasError() { return lastType == "Error"; }
	bool isConnected() { return conn != NULL; }
	const char* getLastType() { return lastType.c_str(); }

private:
	std::string hostname;
	int port;
	std::string lastType;
	SOCKET conn;
};

#endif // PROTOCOL_H