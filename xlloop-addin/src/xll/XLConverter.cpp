/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "XLConverter.h"
#include "../xll/XLUtil.h"

Variant* XLConverter::ConvertXArray(LPXLOPER x, bool freex)
{
	int rows = x->val.array.rows;
	int cols = x->val.array.columns;
	if(rows == 1 && cols == 1) {
		return ConvertX(&x->val.array.lparray[0]);
	}

	VTCollection* colr = new VTCollection();
	if(cols == 1) {
		for(int i = 0; i < rows; i++) {
			Variant* v = ConvertX(&(x->val.array.lparray[i]));
			colr->add(v);
		}
	} else {
		for(int i = 0; i < rows; i++) {
			VTCollection* colc = new VTCollection();
			for(int j = 0; j < cols; j++) {
				Variant* v = ConvertX(&(x->val.array.lparray[i * cols + j]));
				colc->add(v);
			}
			colr->add(colc);
		}
	}

	if(freex) {
		Excel4(xlFree, 0, 1, x);
	}

	return colr;
}

Variant* XLConverter::ConvertX(LPXLOPER x, bool missingToNull)
{
	if(x == NULL) return NULL;

	switch(x->xltype & ~(xlbitXLFree | xlbitDLLFree))
	{
	case xltypeBigData: 
		return NULL;
	case xltypeBool: 
		return new VTBoolean(x->val.boolean);
	case xltypeErr: 
		return NULL;
	case xltypeFlow: 
		return NULL;
	case xltypeInt: 
		return new VTInteger(x->val.w);
	case xltypeMissing: 
		return missingToNull ? new VTNull() : NULL;
	case xltypeMulti: 
		return ConvertXArray(x);
	case xltypeNil: 
		return NULL;
	case xltypeNum: 
		return new VTDouble(x->val.num);
	case xltypeRef: 
	case xltypeSRef: 
		{
			XLOPER xMulti;
			XLOPER xTempMulti;
			xTempMulti.xltype = xltypeInt;
			xTempMulti.val.w = xltypeMulti;

			if(Excel4( xlCoerce, (LPXLOPER) &xMulti, 2, (LPXLOPER)x, (LPXLOPER)&xTempMulti ) != xlretUncalced) {
				return ConvertXArray(&xMulti, true);
			}
		}
	case xltypeStr: 
		{
			if(x->val.str == NULL) {
				return new VTString("");
			} else {
				char chars[MAX_PATH];
				int len = x->val.str[0];
				if(len > 254 || len < 0) {
					len = 254;
				}
				memcpy(chars, &x->val.str[1], len);
				chars[len] = 0;
				return new VTString(chars);
			}
		}
	}

	return NULL;
}

LPXLOPER XLConverter::ConvertV(const Variant* v, LPXLOPER xl)
{
	if(v == NULL) return NULL;
	if(xl == NULL) xl = (LPXLOPER) malloc(sizeof(XLOPER));

	switch(v->getType()) {
	case VSTRUCT:
		{
			// Create 2 column table with name as first column and value as second
			VTStruct* struc = (VTStruct*) v;
			int rows = struc->size();
			xl->val.array.rows = rows;
			xl->val.array.columns = 2;
			xl->val.array.lparray = (LPXLOPER) malloc(sizeof(XLOPER) * rows * 2);
			for(int i = 0; i < rows; i++) {
				const char* key = struc->getKey(i);
				const Variant* vi = struc->getValue(i);
				xl->val.array.lparray[i << 1].xltype = xltypeStr;
				xl->val.array.lparray[i << 1].val.str = XLUtil::MakeExcelString(key);
				XLOPER txl;
				ConvertV(vi, (LPXLOPER) &txl);
				memcpy(&(xl->val.array.lparray[(i << 1) + 1]), (LPXLOPER) &txl, sizeof(XLOPER));
			}
			xl->xltype = xltypeMulti;
		}
		break;
	case VCOLLECTION:
		{
			VTCollection* coll = (VTCollection*) v;
			int rows = coll->size();
			if(rows == 0) {
				xl->xltype = xltypeMissing;
				break;
			}
			Variant* v0 = coll->get(0);
			if(v0->getType() == VCOLLECTION) {
			int cols = ((VTCollection*)v0)->size();
			xl->val.array.rows = rows;
			xl->val.array.columns = cols;
			xl->val.array.lparray = (LPXLOPER) malloc(sizeof(XLOPER) * rows * cols);
			for(int i = 0; i < rows; i++) {
				Variant* vi = coll->get(i);
				if(vi == NULL || v->getType() != VCOLLECTION) {
					free(xl->val.array.lparray);
					free(xl);
					return NULL;
				}
				VTCollection* vci = (VTCollection*) vi;
				int csize = vci->size();
				for(int j = 0; j < csize; j++) {
					LPXLOPER pxl = ConvertV(vci->get(j));
					memcpy((LPXLOPER) &(xl->val.array.lparray[cols * i + j]), (LPXLOPER) pxl, sizeof(XLOPER));
					free(pxl);
				}
			}
			}
			else {
				xl->val.array.rows = rows;
				xl->val.array.columns = 1;
				xl->val.array.lparray = (LPXLOPER) malloc(sizeof(XLOPER) * rows);
				for(int i = 0; i < rows; i++) {
					LPXLOPER pxl = ConvertV(coll->get(i));
					memcpy((LPXLOPER) &(xl->val.array.lparray[i]), (LPXLOPER) pxl, sizeof(XLOPER));
					free(pxl);
				}				
			}

			xl->xltype = xltypeMulti;
		}
		break;
	case VSTRING:
		xl->xltype = xltypeStr;
		xl->val.str = XLUtil::MakeExcelString(((VTString*)v)->get());
		break;
	case VFLOAT:
		xl->xltype = xltypeNum;
		xl->val.num = ((VTFloat*)v)->get();
		break;
	case VDOUBLE:
		xl->xltype = xltypeNum;
		xl->val.num = ((VTDouble*)v)->get();
		break;
	case VBOOLEAN:
		xl->xltype = xltypeBool;
		xl->val.num = ((VTBoolean*)v)->get();
		break;
	case VBYTE:
		xl->xltype = xltypeNum;
		xl->val.num = ((VTByte*)v)->get();
		break;
	case VSHORT:
		xl->xltype = xltypeNum;
		xl->val.num = ((VTShort*)v)->get();
		break;
	case VINTEGER:
		xl->xltype = xltypeNum;
		xl->val.num = ((VTInteger*)v)->get();
		break;
	case VLONG:
		xl->xltype = xltypeNum;
		xl->val.num = ((VTLong*)v)->get();
		break;
	case VNULL:
		xl->xltype = xltypeMissing;
		break;
	default:
		xl->xltype = xltypeStr;
		xl->val.str = " #Unknown Error";
		break;
	}

	xl->xltype |= xlbitXLFree;

	return xl;
}
