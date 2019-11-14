/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/

#ifndef JSON_CODEC_H
#define JSON_CODEC_H

#include "../yajl/yajl_common.h"
#include "../yajl/yajl_parse.h"
#include "../yajl/yajl_gen.h"
#include <windows.h>
#include "xlcall.h"

#define JSON_TYPE_MAP 1
#define JSON_TYPE_ARRAY 2
#define JSON_TYPE_STR 3
#define JSON_TYPE_NUM 4
#define JSON_TYPE_INT 5
#define JSON_TYPE_BOOL 6
#define JSON_TYPE_NULL 7

typedef struct _jtv {
	int type;
	char* str;
	double num;
	int i;
	bool b;
	struct {
		struct _jtv* elem;
	} array;
	struct {
		struct _jtv* key;
		struct _jtv* value;
	} map;
	struct _jtv* parent;
} json_value;

typedef struct _jctx {
	json_value* current;
	char* key;
} json_ctx;


class JSONCodec {
public:
	static void Encode(yajl_gen g, LPXLOPER x);
	static void FreeJsonValue(json_value* v);
	static bool Decode(json_value* v, LPXLOPER x);
	static yajl_handle AllocateHandle(json_ctx* ctx);
};

#endif // JSON_CODEC_H