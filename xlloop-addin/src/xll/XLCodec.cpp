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
	writeDoubleWord(type, os);
	switch(type) {
		case xltypeBool:
			os.put(xl->val.boolean);
			break;
		case xltypeErr:
			writeDoubleWord(xl->val.err, os);
			break;
		case xltypeInt:
			writeDoubleWord(xl->val.w, os);
			break;
		case xltypeMulti:
			writeDoubleWord(xl->val.array.rows, os);
			writeDoubleWord(xl->val.array.columns, os);
			len = xl->val.array.rows*xl->val.array.columns;
			for(int i = 0; i < len; i++) {
				encode(&xl->val.array.lparray[i], os);
			}
			break;
		case xltypeNum:
			writeDouble(xl->val.num, os);
			break;
		case xltypeStr:
			os.put(xl->val.str[0]);
			os.write(&xl->val.str[1], xl->val.str[0]);
			break;
		case xltypeMissing:
		case xltypeNil:
			break;
	}
}

void XLCodec::encode(const char* str, std::ostream& os)
{
	writeDoubleWord(xltypeStr, os);
	int len = strlen(str);
	os.put(len);
	os.write(str, len);
}

void XLCodec::encode(int w, std::ostream& os)
{
	writeDoubleWord(xltypeInt, os);
	writeDoubleWord(w, os);
}

void XLCodec::decode(std::istream& is, LPXLOPER xl) 
{
	xl->xltype = readDoubleWord(is);
	int len;
	switch(xl->xltype) {
		case xltypeBool:
			xl->val.boolean = is.get();
			break;
		case xltypeErr:
			xl->val.err = readDoubleWord(is);
			break;
		case xltypeInt:
			xl->val.w = readDoubleWord(is);
			break;
		case xltypeMulti:
			xl->val.array.rows = readDoubleWord(is);
			xl->val.array.columns = readDoubleWord(is);
			len = xl->val.array.rows * xl->val.array.columns;
			xl->val.array.lparray = new XLOPER[len];
			for(int i = 0; i < len; i++) {
				decode(is, &xl->val.array.lparray[i]);
			}
			break;
		case xltypeNum:
			xl->val.num = readDouble(is);
			break;
		case xltypeStr:
			len = is.get();
			xl->val.str = new char[len+1];
			xl->val.str[0] = (char) len;
			is.read(&xl->val.str[1], len);
			break;
		case xltypeMissing:
		case xltypeNil:
			break;
		default:
			xl->xltype = xltypeErr;
			xl->val.err = xlerrNA;
			break;
	}
	xl->xltype |= xlbitXLFree;
}