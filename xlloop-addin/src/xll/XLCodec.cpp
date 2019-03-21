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
	const WCHAR* g_CurrentFunction;
}

inline void XOStream::put(char c)
{
	if(pos >= STREAM_BUF_SIZE)
		flush();
	buf[pos++] = c;
}

inline void XOStream::write(const char* utf8, unsigned int n)
{
	if(n <= 0) return;
	if(n > STREAM_BUF_SIZE) {
		unsigned int i = 0;
		while(i < n && s) {
			unsigned int size = STREAM_BUF_SIZE - pos;
			if(size > n)
				size = n;
			memcpy(&buf[pos], &utf8[i], size);
			i += size;
			if(pos >= STREAM_BUF_SIZE)
				flush();
		}
	} else {
		if(pos + n >= STREAM_BUF_SIZE)
			flush();
		memcpy(&buf[pos], utf8, n);
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

void XOStream::reset()
{
	pos=0;
}

inline int XIStream::get()
{
	if(s == 0)
		return -1;
	if(pos >= len)
		fill();
	return buf[pos++] & 0xff;
}

inline void XIStream::read(char* utf8, UINT n)
{
	if(utf8 == 0 || n <= 0)
		return;

	if(pos >= len)
		fill();

	int i = 0;
	while(n > 0 && utf8) {
		UINT size = len-pos;
		if(size > n)
			size = n;
		if(size > 0) {
			memcpy(&utf8[i], &buf[pos], size);
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
			int error = WSAGetLastError();
			if(error != WSAETIMEDOUT)
				break;
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

void XIStream::reset()
{
	pos = 0;
	len = 0;
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

void XLCodec::encode(const LPXLOPER12 xl, XOStream& os)
{
	int type = xl->xltype & ~(xlbitXLFree | xlbitDLLFree);
	UINT len;
	WCHAR* str;
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
			// Convert to UTF8
			str = xl->val.str;
			len = (unsigned short) str[0];
			if (len > 0)
			{
				char* utf8 = (char*) malloc(len * 2);
				memset(utf8, 0, len * 2);
				len = WideCharToMultiByte(CP_UTF8, 0, &str[1], len, utf8, len * 2, 0, 0);
				os.put(len);
				os.write(utf8, len);
			}
			else
				os.put(len);
			break;
		case xltypeNil:
			os.put(XL_CODEC_TYPE_NIL);
			break;
		case xltypeSRef:
			os.put(XL_CODEC_TYPE_SREF);
			writeDoubleWord(xl->val.sref.ref.colFirst, os);
			writeDoubleWord(xl->val.sref.ref.colLast, os);
			writeDoubleWord(xl->val.sref.ref.rwFirst, os);
			writeDoubleWord(xl->val.sref.ref.rwLast, os);
			break;
		case xltypeMissing:
		default:
			os.put(XL_CODEC_TYPE_MISSING);
			break;
	}
}

void XLCodec::encode(const char* utf8, XOStream& os)
{
	os.put(XL_CODEC_TYPE_STR);
	if(utf8) {
		int len = strlen(utf8);
		os.put(len);
		os.write(utf8, len);
	} else {
		os.put(0);
	}
}

void XLCodec::encode(bool b, XOStream& os)
{
	os.put(XL_CODEC_TYPE_BOOL);
	os.put(b?1:0);
}

void XLCodec::encode(int w, XOStream& os)
{
	os.put(XL_CODEC_TYPE_INT);
	writeDoubleWord(w, os);
}

void XLCodec::decode(const WCHAR* name, XIStream& is, LPXLOPER12 xl)
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
				xl->val.array.lparray = (LPXLOPER12) malloc(sizeof(XLOPER12) * len);
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
			if (is.valid()) {
				if (len > 0) {
					// Read UTF8 and convert to WCHAR
					char* utf8 = new char[len];
					is.read(utf8, len);
					int wlen = MultiByteToWideChar(CP_UTF8, 0, utf8, len, NULL, 0);
					xl->val.str = new WCHAR[len + 1];
					xl->val.str[0] = (WCHAR) wlen;
					MultiByteToWideChar(CP_UTF8, 0, utf8, len, &xl->val.str[1], len);
				}
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