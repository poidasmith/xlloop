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
#include "../xll/XLUtil.h"
#include <istream>

#define XL_CODEC_TYPE_NUM 0x1
#define XL_CODEC_TYPE_STR 0x2
#define XL_CODEC_TYPE_BOOL 0x3
#define XL_CODEC_TYPE_ERR 0x4
#define XL_CODEC_TYPE_MULTI 0x5
#define XL_CODEC_TYPE_MISSING 0x6
#define XL_CODEC_TYPE_NIL 0x7
#define XL_CODEC_TYPE_INT 0x8

inline void writeDoubleWord(unsigned int value, std::ostream& os)
{
	os.put(value >> 24 & 0xff);
	os.put(value >> 16 & 0xff);
	os.put(value >> 8 & 0xff);
	os.put(value & 0xff);
}

inline void writeDouble(double value, std::ostream& os)
{
	unsigned int* p = (unsigned int *)&value;
	writeDoubleWord(p[1], os);
	writeDoubleWord(p[0], os);
}

inline int readDoubleWord(std::istream& is) 
{
	return is.get() << 24 | is.get() << 16 | is.get() << 8 | is.get();
}

inline double readDouble(std::istream& is)
{
	int v1 = readDoubleWord(is);
	int v2 = readDoubleWord(is);
	double val;
	unsigned int* p = (unsigned int*)&val;
	p[1] = v1;
	p[0] = v2;
	return val;
}

void XLCodec::encode(const LPXLOPER xl, std::ostream& os)
{
	int type = xl->xltype & ~(xlbitXLFree | xlbitDLLFree);
	int len;
	switch(type) {
		case xltypeBool:
			os.put(XL_CODEC_TYPE_BOOL);
			os.put(xl->val.boolean);
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
			for(int i = 0; i < len; i++) {
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
			os.write(&xl->val.str[1], xl->val.str[0]);
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

void XLCodec::encode(const char* str, std::ostream& os)
{
	os.put(XL_CODEC_TYPE_STR);
	int len = strlen(str);
	os.put(len);
	os.write(str, len);
}

void XLCodec::encode(int w, std::ostream& os)
{
	os.put(XL_CODEC_TYPE_INT);
	writeDoubleWord(w, os);
}

void XLCodec::decode(std::istream& is, LPXLOPER xl) 
{
	int type = is.get();
	int len;
	switch(type) {
		case XL_CODEC_TYPE_BOOL:
			xl->xltype = xltypeBool;
			xl->val.boolean = is.get();
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
			len = xl->val.array.rows * xl->val.array.columns;
			xl->val.array.lparray = new XLOPER[len];
			for(int i = 0; i < len; i++) {
				decode(is, &xl->val.array.lparray[i]);
			}
			break;
		case XL_CODEC_TYPE_NUM:
			xl->xltype = xltypeNum;
			xl->val.num = readDouble(is);
			break;
		case XL_CODEC_TYPE_STR:
			xl->xltype = xltypeStr;
			len = is.get();
			xl->val.str = new char[len+1];
			xl->val.str[0] = (char) len;
			is.read(&xl->val.str[1], len);
			break;
		case XL_CODEC_TYPE_MISSING:
			xl->xltype = xltypeMissing;
			break;
		case XL_CODEC_TYPE_NIL:
			xl->xltype = xltypeNil;
			break;
		default:
			xl->xltype = xltypeErr;
			xl->val.err = xlerrNA;
			break;
	}
	xl->xltype |= xlbitXLFree;
}