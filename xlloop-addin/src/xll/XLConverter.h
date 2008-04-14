/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef XL_CONVERTER_H
#define XL_CONVERTER_H

#include "../common/Runtime.h"
#include "../xll/xlcall.h"
#include "../common/VTCodec.h"

class XLConverter {
public:
	static Variant* ConvertX(LPXLOPER x, bool missingToNull = true);
	static LPXLOPER ConvertV(const Variant* v, LPXLOPER xl = NULL);

private:
	static Variant* ConvertXArray(LPXLOPER x, bool freex = false);
};

#endif // XL_CONVERTER_H