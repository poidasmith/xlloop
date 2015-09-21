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

#define XL_CODEC_TYPE_NUM 0x1
#define XL_CODEC_TYPE_STR 0x2
#define XL_CODEC_TYPE_BOOL 0x3
#define XL_CODEC_TYPE_ERR 0x4
#define XL_CODEC_TYPE_MULTI 0x5
#define XL_CODEC_TYPE_MISSING 0x6
#define XL_CODEC_TYPE_NIL 0x7
#define XL_CODEC_TYPE_INT 0x8
#define XL_CODEC_TYPE_SREF 0x9

class XOStream {
public:
	XOStream(SOCKET& s) : s(s), pos(0) {}
	void put(char c);
	void write(const char* s, UINT n);
	void flush();
	void reset();

private:
	char buf[STREAM_BUF_SIZE];
	int pos;
	SOCKET& s;
};

class XIStream {
public:
	XIStream(SOCKET& s) : s(s), pos(0), len(0) {}
	int get();
	void read(char* s, UINT n);
	inline bool valid() { return s != NULL; }
	void reset();

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
	static void encode(bool b, XOStream& os);
	static void encode(int w, XOStream& os);
	static void decode(const char* name, XIStream& is, LPXLOPER xl);
};

#endif // XL_CODEC_H