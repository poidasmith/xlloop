/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "XLCodec.h"
#include "XLUtil.h"
#include "Timeout.h"

#define SOCKET_TIMEOUT -1

namespace 
{
	const char* g_CurrentFunction;
}

inline void XOStream::put(char c)
{
	if(pos >= STREAM_BUF_SIZE)
		flush();
	buf[pos++] = c;
}

inline void XOStream::write(const char* s, unsigned int n)
{
	if(n <= 0) return;
	if(n > STREAM_BUF_SIZE) {
		unsigned int i = 0;
		while(i < n && s) {
			unsigned int size = STREAM_BUF_SIZE - pos;
			if(size > n)
				size = n;
			memcpy(&buf[pos], &s[i], size);
			i += size;
			if(pos >= STREAM_BUF_SIZE)
				flush();
		}
	} else {
		if(pos + n >= STREAM_BUF_SIZE)
			flush();
		memcpy(&buf[pos], s, n);
		pos += n;
	}
}

inline void XOStream::flush()
{
	if(pos > 0) {
		int len = 0;
		int i = 0;
		while(s && i < pos) {
			len = send(s, &buf[i], pos-i, 0); // never sure how much is sent
			if(len < 0) {
				s = 0;
				break;
			}
			i += len;
		}
		pos = 0;
	}
}

inline int XIStream::get()
{
	if(s == 0)
		return -1;
	if(pos >= len)
		fill();
	return buf[pos++] & 0xff;
}

inline void XIStream::read(char* s, UINT n)
{
	if(s == 0 || n <= 0)
		return;

	if(pos >= len)
		fill();

	int i = 0;
	while(n > 0 && s) {
		UINT size = len-pos;
		if(size > n)
			size = n;
		if(size > 0) {
			memcpy(&s[i], &buf[pos], size);
			pos += size;
		}
		n -= size;
		i += size;
		if(n > 0 && pos >= len)
			fill(); // pos=0, len=0..STREAM_BUF_SIZE
	}
}

inline void XIStream::fill()
{
	int r = recv(s, buf, STREAM_BUF_SIZE, 0);
	if(r == SOCKET_TIMEOUT) {
		int c = 0;
		Timeout::Init();
		while((r = recv(s, buf, STREAM_BUF_SIZE, 0)) == SOCKET_TIMEOUT) {
			if(c == 2) {
				Timeout::Show(g_CurrentFunction);
			} else if(c > 2 && Timeout::UserCancelled()) {
				closesocket(s);
				s = 0;
				break;
			}
			c++;
		}
		Timeout::Cleanup();
	}
	if(r > 0) {
		len = r;
	} else {
		len = 0;
		s = 0;
	}

	pos = 0;
}

inline void writeDoubleWord(unsigned int value, XOStream& os)
{
	os.put(value >> 24 & 0xff);
	os.put(value >> 16 & 0xff);
	os.put(value >> 8 & 0xff);
	os.put(value & 0xff);
}

inline void writeDouble(double value, XOStream& os)
{
	unsigned int* p = (unsigned int *)&value;
	writeDoubleWord(p[1], os);
	writeDoubleWord(p[0], os);
}

inline int readDoubleWord(XIStream& is) 
{
	return is.get() << 24 | is.get() << 16 | is.get() << 8 | is.get();
}

inline double readDouble(XIStream& is)
{
	int v1 = readDoubleWord(is);
	int v2 = readDoubleWord(is);
	double val;
	unsigned int* p = (unsigned int*)&val;
	p[1] = v1;
	p[0] = v2;
	return val;
}

void XLCodec::encode(const LPXLOPER xl, XOStream& os)
{
	int type = xl->xltype & ~(xlbitXLFree | xlbitDLLFree);
	UINT len;
	int t = 0;
	switch(type) {
		case xltypeBool:
			os.put(XL_CODEC_TYPE_BOOL);
			os.put(xl->val.xbool);
			break;
		case xltypeErr:
			os.put(XL_CODEC_TYPE_ERR);
			writeDoubleWord(xl->val.err, os);
			break;
		case xltypeInt:
			os.put(XL_CODEC_TYPE_INT);
			writeDoubleWord(xl->val.w, os);
			break;
		case xltypeMulti:
			os.put(XL_CODEC_TYPE_MULTI);
			writeDoubleWord(xl->val.array.rows, os);
			writeDoubleWord(xl->val.array.columns, os);
			len = xl->val.array.rows*xl->val.array.columns;
			for(UINT i = 0; i < len; i++) {
				encode(&xl->val.array.lparray[i], os);
			}
			break;
		case xltypeNum:
			os.put(XL_CODEC_TYPE_NUM);
			writeDouble(xl->val.num, os);
			break;
		case xltypeStr:
			os.put(XL_CODEC_TYPE_STR);
			os.put(xl->val.str[0]);
			os.write(&xl->val.str[1], (unsigned char) xl->val.str[0]);
			break;
		case xltypeNil:
			os.put(XL_CODEC_TYPE_NIL);
			break;
		case xltypeMissing:
		default:
			os.put(XL_CODEC_TYPE_MISSING);
			break;
	}
}

void XLCodec::encode(const char* str, XOStream& os)
{
	os.put(XL_CODEC_TYPE_STR);
	int len = strlen(str);
	os.put(len);
	os.write(str, len);
}

void XLCodec::encode(int w, XOStream& os)
{
	os.put(XL_CODEC_TYPE_INT);
	writeDoubleWord(w, os);
}

void XLCodec::decode(const char* name, XIStream& is, LPXLOPER xl) 
{
	g_CurrentFunction = name;
	int type = is.get();
	UINT len;
	switch(type) {
		case XL_CODEC_TYPE_BOOL:
			xl->xltype = xltypeBool;
			xl->val.xbool = is.get();
			break;
		case XL_CODEC_TYPE_ERR:
			xl->xltype = xltypeErr;
			xl->val.err = readDoubleWord(is);
			break;
		case XL_CODEC_TYPE_INT:
			xl->xltype = xltypeInt;
			xl->val.w = readDoubleWord(is);
			break;
		case XL_CODEC_TYPE_MULTI:
			xl->xltype = xltypeMulti;
			xl->val.array.rows = readDoubleWord(is);
			xl->val.array.columns = readDoubleWord(is);
			if(is.valid()) {
				len = xl->val.array.rows * xl->val.array.columns;
				xl->val.array.lparray = (LPXLOPER) malloc(sizeof(XLOPER) * len);
				for(UINT i = 0; i < len; i++) {
					decode(name, is, &xl->val.array.lparray[i]);
				}
			}
			break;
		case XL_CODEC_TYPE_NUM:
			xl->xltype = xltypeNum;
			xl->val.num = readDouble(is);
			break;
		case XL_CODEC_TYPE_STR:
			xl->xltype = xltypeStr;
			len = is.get();
			if(is.valid()) {
				xl->val.str = new char[len+1];
				xl->val.str[0] = (char) len;
				if(len > 0)
					is.read(&xl->val.str[1], len);
			}
			break;
		case XL_CODEC_TYPE_MISSING:
			xl->xltype = xltypeMissing;
			xl->val.str = 0;
			break;
		case XL_CODEC_TYPE_NIL:
			xl->xltype = xltypeNil;
			xl->val.str = 0;
			break;
		default:
			xl->xltype = xltypeErr;
			xl->val.err = xlerrNA;
			break;
	}

	// Check for valid socket
	if(!is.valid())
		xl->xltype = xltypeNil;

	xl->xltype |= xlbitDLLFree;
}