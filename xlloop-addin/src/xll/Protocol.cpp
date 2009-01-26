/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "Protocol.h"

#include <stdio.h>
#include <iostream>
using namespace std;

class tcpbuf : public streambuf 
{
public:
	explicit tcpbuf(SOCKET& conn) : conn(conn) {}
	virtual ~tcpbuf() {}

	int write(const char* buf, const int n) 
	{ 
		int res = ::send(conn, buf, n, 0);
		if(res < 0) {
			conn = NULL;
		}
		return n; 
	}
	int read(char* buf, const int n) 
	{
		int res = ::recv(conn, buf, n, 0);
		if(res < 0) {
			conn = NULL;
		}
		return res;
	}

protected:
	virtual int overflow(int c) 
	{
		if(sync() == EOF) {
			return EOF;
		}

		if(pbase() == 0) {
			doallocate();
		}

		if(c != EOF) {
			*pptr() = c;
			pbump(1);
		}

		return 0;
	}
	virtual int underflow()
	{
		if(gptr() < egptr())
			return *(unsigned char*)gptr();

		if(sync() == EOF)
			return EOF;

		if(pbase() == 0)
			doallocate();

		const int count = read(pbase(), egptr() - pbase());
		setg(pbase(), pbase(), pbase() + (count <= 0 ? 0 : count));
		setp(pbase(), pbase());
		return count <= 0 ? EOF : *(unsigned char*)gptr();
	}
	virtual int sync(void) 
	{
		const int n = pptr() - pbase();
		if(n == 0) {
			return 0;
		}
		return write(pbase(), n) == n ? (pbump(-n), 0) : EOF;	
	}
	virtual int doallocate(void) 
	{
		const int size = 512;
		char *p = (char *) malloc(size);
		setp(p, p+size);
		setg(p, p, p+size);
		return 1;
	}

private:
	SOCKET& conn;
};

class tcpstream : public iostream
{
public:
	tcpstream(tcpbuf* tbuf) : iostream(tbuf), buf(tbuf) {}
	~tcpstream() {}
	tcpbuf* rdbuf(void) const { 
		return buf; 
	}
	bool is_open() const { return true; }

private:
	mutable tcpbuf* buf;
};

int Protocol::connect()
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
	const char* servername = hostname.c_str();
	if(inet_addr(servername) == INADDR_NONE) {
		hp = gethostbyname(servername);
	}
	else {
		unsigned int addr = inet_addr(servername);
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

	if(::connect(conn, (struct sockaddr*) &server, sizeof(server))) {
		closesocket(conn);
		conn = NULL;
		return 1;	
	}

	return 0;
}

void Protocol::disconnect()
{
	if(conn) {
		closesocket(conn);
		conn = NULL;
	}

	WSACleanup();
}

int Protocol::send(const char* name, LPXLOPER v0, LPXLOPER v1, LPXLOPER v2, LPXLOPER v3, 
		LPXLOPER v4, LPXLOPER v5, LPXLOPER v6, LPXLOPER v7, LPXLOPER v8, LPXLOPER v9)
{
	tcpbuf b(conn);
	tcpstream s(&b);
	XLCodec::encode(name, s);
	XLCodec::encode(10, s);
	XLCodec::encode(v0, s);
	XLCodec::encode(v1, s);
	XLCodec::encode(v2, s);
	XLCodec::encode(v3, s);
	XLCodec::encode(v4, s);
	XLCodec::encode(v5, s);
	XLCodec::encode(v6, s);
	XLCodec::encode(v7, s);
	XLCodec::encode(v8, s);
	XLCodec::encode(v9, s);
	s.flush();
	return conn == NULL ? 1 : 0;
}

int Protocol::send(const char* name, int count, LPXLOPER v)
{
	tcpbuf b(conn);
	tcpstream s(&b);
	XLCodec::encode(name, s);
	XLCodec::encode(count, s);
	for(int i = 0; i < count; i++) {
		XLCodec::encode(&v[i], s);
	}
	s.flush();
	return conn == NULL ? 1 : 0;
}

int Protocol::send(const char* name)
{
	tcpbuf b(conn);
	tcpstream s(&b);
	XLCodec::encode(name, s);
	XLCodec::encode(0, s);
	s.flush();
	return conn == NULL ? 1 : 0;
}

LPXLOPER Protocol::receive()
{
	tcpbuf b(conn);
	tcpstream s(&b);
	LPXLOPER xl = new XLOPER;
	XLCodec::decode(s, xl);
	return xl;
}
