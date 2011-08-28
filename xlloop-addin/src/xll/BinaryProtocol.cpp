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
#include "XLUtil.h"
#include <stdio.h>
#include "../Common/Log.h"

#define PROTOCOL_VERSION 20
#define MAX_SERVERS 20
#define FS_SERVER_LIST ":server"
#define FS_SERVER_SELECTION_MODE ":server.selection.mode"
#define FS_SERVER_RETRY_COUNT ":server.retry.count"

void BinaryProtocol::initialize(dictionary* ini, const char* section)
{
	errorMessage.xltype = xltypeStr;
	errorMessage.val.str = XLUtil::MakeExcelString("#Cannot connect to server");
	char* server = INI::GetString(ini, section, FS_SERVER_LIST, NULL);
	this->servers = (char**) malloc(sizeof(char*) * MAX_SERVERS);
	this->serverPorts = (int*) malloc(sizeof(int) * MAX_SERVERS);
	this->selectedServer = 0;
	this->selectionMode = SERVER_SELECT_MODE_RANDOM;
	this->retryCount = iniparser_getint(ini, FS_SERVER_RETRY_COUNT, 0);
	char* selMode = iniparser_getstr(ini, FS_SERVER_SELECTION_MODE);
	if(selMode) {
		if(strcmp("round-robin", selMode) == 0) {
			this->selectionMode = SERVER_SELECT_MODE_ROUND_ROBIN;
			Log::Info("BinaryProtocol - using round-robin server selection mode");
		} else {
			Log::Info("BinaryProtocol - unrecognized server selection mode: %s", selMode);
		}
	}
	ParseServerList(server);
}

int BinaryProtocol::connect()
{
	// Check if we are already connected
	if(conn != NULL) return 0;
	if(serverCount == 0) return 1;
	if(serverCount == 1) return connect(servers[0], serverPorts[0]);

	int res = connect(selectionMode);

	// Any further selections go round the list
	int retry = this->retryCount;
	while(res && retry--)
		res = connect(SERVER_SELECT_MODE_ROUND_ROBIN);

	return res;
}

int BinaryProtocol::connect(int selectionMode)
{
	// If not connected then choose a server to connect to
	switch(selectionMode) {
	case SERVER_SELECT_MODE_RANDOM:
		this->selectedServer = GetTickCount() % this->serverCount;
		break;
	case SERVER_SELECT_MODE_ROUND_ROBIN:
		if(this->selectedServer >= this->serverCount)
			this->selectedServer = 0;
		break;
	}

	char temp[MAX_PATH];
	sprintf(temp, "#Cannot connect to server [%d] - %s:%d", selectedServer+1, 
		servers[selectedServer], serverPorts[selectedServer]);
	if(errorMessage.val.str) free(errorMessage.val.str);
	errorMessage.val.str= XLUtil::MakeExcelString(temp);

	char* hostname = servers[selectedServer];
	int port = serverPorts[selectedServer];
	Log::Info("Using server: %s:%d", hostname, port);

	// Update the selected server for next time
	this->selectedServer++;

	return connect(hostname, port);
}

int BinaryProtocol::connect(char* hostname, int port)
{
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

LPXLOPER BinaryProtocol::execute(const char* name, bool sendCaller, int count, ...)
{
	va_list args;
	va_start(args, count);
	send(name, sendCaller, count, (LPXLOPER*) args);
	va_end(args);
	return receive(name);
}

LPXLOPER BinaryProtocol::execute(const char* name, bool sendCaller, int count, LPXLOPER far opers[])
{
	send(name, sendCaller, count, opers);
	return receive(name);
}

int BinaryProtocol::send(const char* name, bool sendCaller, int count, LPXLOPER far opers[])
{
	XLCodec::encode(PROTOCOL_VERSION, os);
	XLCodec::encode(sendCaller, os);
	if(sendCaller) {
		XLOPER xlRef, xlSheetName;
		Excel4(xlfCaller, &xlRef, 0);	
		Excel4(xlSheetNm, &xlSheetName, 1, &xlRef);
		XLCodec::encode(&xlRef, os);
		XLCodec::encode(&xlSheetName, os);
		Excel4(xlFree, 0, 1, &xlSheetName);
	}
	XLCodec::encode(name, os);
	// Find last non-missing value
	while(count>0) {
		if((opers[count-1]->xltype & ~(xlbitXLFree | xlbitDLLFree)) == xltypeMissing)
			count--;
		else
			break;
	}
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

int BinaryProtocol::ExtractPort(char* server)
{
	int len = strlen(server);
	for(int i = 0; i < len; i++) {
		if(server[i] == ':') {
			server[i] = 0;
			return atoi(&server[i+1]);
		}
	}

	return 5454;
}

void BinaryProtocol::ParseServerList(char* server)
{
	if(server==NULL) {
		servers[0] = strdup("localhost");
		serverPorts[0] = 5454;
		serverCount = 1;
		selectedServer = 0;
		return;
	}

	int len = strlen(server);
	int start = 0;
	int pos = 0;
	char temp[MAX_PATH];
	for(pos = 0; pos < len+1; pos++) {
		if(serverCount >= MAX_SERVERS)
			break;
		if(server[pos] == ',' || pos==len) {
			int slen = pos-start;
			if(slen==0) {
				start = pos;
				continue;
			}
			memcpy(temp, &server[start], slen+1);
			temp[slen]=0;
			StrTrim(temp, ", ");
			servers[serverCount]=strdup(temp);
			serverPorts[serverCount] = ExtractPort(servers[serverCount]);
			serverCount++;
			start = pos;
		}
	}
}
