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

#include <vector>
#include <map>
#include <windows.h>
#include "../xll/xlcall.h"

class XLCodec {
public:
	static void encode(const LPXLOPER xl, std::ostream& os);
	static void encode(const char* str, std::ostream& os);
	static void encode(int w, std::ostream& os);
	static void decode(std::istream& is, LPXLOPER xl);
};

#endif // XL_CODEC_H