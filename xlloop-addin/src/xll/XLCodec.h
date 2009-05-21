/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XL_CODEC_H
#define XL_CODEC_H

#include <windows.h>
#include "../xll/xlcall.h"

#define STREAM_BUF_SIZE 4096

class XOStream {
public:
	XOStream(SOCKET& s) : s(s), pos(0) {}
	void put(char c);
	void write(const char* s, int n);
	void flush();

private:
	char buf[STREAM_BUF_SIZE];
	int pos;
	SOCKET& s;
};

class XIStream {
public:
	XIStream(SOCKET& s) : s(s), pos(0), len(0) {}
	int get();
	void read(char* s, int n);
	inline bool valid() { return s != NULL; }

private:
	void fill();

private:
	char buf[STREAM_BUF_SIZE];
	int pos;
	int len;
	SOCKET& s;
};

class XLCodec {
public:
	static void encode(const LPXLOPER xl, XOStream& os);
	static void encode(const char* str, XOStream& os);
	static void encode(int w, XOStream& os);
	static void decode(const char* name, XIStream& is, LPXLOPER xl);
};

#endif // XL_CODEC_H