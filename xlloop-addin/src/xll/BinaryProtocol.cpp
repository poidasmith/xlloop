/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "BinaryProtocol.h"

#include <stdio.h>

#define PROTOCOL_VERSION 20

int BinaryProtocol::connect()
{
	if(conn != NULL) return 0;

	WSADATA wsData;
	int wsaret = WSAStartup(0x101, &wsData);
	if(wsaret)
		return 1;

	conn = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if(conn == INVALID_SOCKET) 
		return 1;

	hostent* hp;
	if(inet_addr(hostname) == INADDR_NONE) {
		hp = gethostbyname(hostname);
	}
	else {
		unsigned int addr = inet_addr(hostname);
		hp = gethostbyaddr((char*)&addr, sizeof(addr), AF_INET);
	}

	if(hp == NULL) {
		closesocket(conn);
		conn = NULL;
		return 1;
	}

	struct sockaddr_in server;
	server.sin_addr.s_addr = *((unsigned long*)hp->h_addr);
	server.sin_family = AF_INET;
	server.sin_port = htons(port);

	// Set timeout
	struct timeval tv;
	tv.tv_sec = 500;
	tv.tv_usec = 0;
	if(setsockopt(conn, SOL_SOCKET, SO_RCVTIMEO, (char *)&tv, sizeof(tv)))
		return 1;

	if(::connect(conn, (struct sockaddr*) &server, sizeof(server))) {
		closesocket(conn);
		conn = NULL;
		return 1;	
	}

	is.reset();
	os.reset();

	return 0;
}

void BinaryProtocol::disconnect()
{
	if(conn) {
		closesocket(conn);
		conn = NULL;
	}

	WSACleanup();
}

LPXLOPER BinaryProtocol::execute(const char* name, int count, ...)
{
	va_list args;
	va_start(args, count);
	send(name, count, (LPXLOPER*) args);
	va_end(args);
	return receive(name);
}

LPXLOPER BinaryProtocol::execute(const char* name, int count, LPXLOPER far opers[])
{
	send(name, count, opers);
	return receive(name);
}

int BinaryProtocol::send(const char* name, int count, LPXLOPER far opers[])
{
	XLCodec::encode(PROTOCOL_VERSION, os);
	XLCodec::encode(sendSourceInfo, os);
	if(sendSourceInfo) {
		XLOPER x;
		Excel4(xlfCaller, &x, 0);
		XLCodec::encode(&x, os);
		Excel4(xlSheetId, &x, 0);
		XLCodec::encode(&x, os);
	}
	XLCodec::encode(name, os);
	XLCodec::encode(count, os);
	for(int i = 0; i < count; i++) {
		XLCodec::encode(opers[i], os);
	}
	os.flush();
	return conn == NULL ? 1 : 0;
}

LPXLOPER BinaryProtocol::receive(const char* name)
{
	LPXLOPER xl = new XLOPER;
	XLCodec::decode(name, is, xl);
	return xl;
}
