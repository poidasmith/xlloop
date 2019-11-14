/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "JSONCodec.h"
#include "XLCodec.h"
#include "../common/Log.h"

json_value* get(json_value* v, char* key)
{
	if(v->type != JSON_TYPE_MAP) 
		return 0;
	json_value* kit = v->map.key;
	json_value* vit = v->map.value;
	while(kit && vit) {
		if(strcmp(kit->str, key) == 0) {
			return vit;
		}
		kit = kit->map.key;
		vit = vit->map.value;
	}
	return 0;
}

json_value* new_value()
{
	json_value* v = (json_value*) malloc(sizeof(json_value));
	memset(v, 0, sizeof(json_value));
	return v;
}

json_value* to_apply(json_ctx* ctx)
{
	if(ctx->current) {
		switch(ctx->current->type) {
			case JSON_TYPE_MAP: {
				json_value* v = new_value();
				json_value* k = new_value();
				k->type = JSON_TYPE_STR;
				k->str = ctx->key;
				ctx->key = NULL;
				// store key
				json_value* it = ctx->current;
				int i = 0;
				while(it->map.key) {
					it = it->map.key;
				}
				it->map.key = k;
				// store value 
				i = 0;
				it = ctx->current;
				while(it->map.value) {
					it = it->map.value;
				}
				it->map.value = v;
				return v;
			}
			break;
			case JSON_TYPE_ARRAY: {
				json_value* v = new_value();
				json_value* it = ctx->current;
				while(it->array.elem) it = it->array.elem;
				it->array.elem = v;
				return v;
			}
		    break;
			default:
			break;
		}
	} else {
		return new_value();
	}
	return 0;
}

void push(json_ctx* ctx, json_value* v)
{
	if(ctx->current)
		v->parent = ctx->current;
	ctx->current = v;
}

void pop(json_ctx* ctx)
{
	if(ctx->current->parent)
		ctx->current = ctx->current->parent;
}

static int cb_null(void * ctx)
{
	json_ctx* c = (json_ctx*) ctx;
	json_value* v = to_apply(c);
	if(!v) return 0;
	v->type = JSON_TYPE_NULL;
    return 1;
}

static int cb_boolean(void * ctx, int boolean)
{
	json_ctx* c = (json_ctx*) ctx;
	json_value* v = to_apply(c);
	if(!v) return 0;
	v->type = JSON_TYPE_BOOL;
	v->b = boolean;
    return 1;
}

static int cb_integer(void * ctx, long long l)
{
	json_ctx* c = (json_ctx*) ctx;
	json_value* v = to_apply(c);
	if(!v) return 0;
	v->type = JSON_TYPE_INT;
	v->i = l;
	return 1;
}

static int cb_double(void * ctx, double d)
{
	json_ctx* c = (json_ctx*) ctx;
	json_value* v = to_apply(c);
	if(!v) return 0;
	v->type = JSON_TYPE_NUM;
	v->num = d;
	return 1;
}

static int cb_string(void * ctx, const unsigned char * s, size_t len)
{
	json_ctx* c = (json_ctx*) ctx;
	json_value* v = to_apply(c);
	if(!v) return 0;
	v->type = JSON_TYPE_STR;
	v->str = (char*) malloc(len + 1);
	memcpy(v->str, s, len);
	v->str[len] = 0;
    return 1;
}

static int cb_map_key(void * ctx, const unsigned char * s, size_t len)
{
	json_ctx* c = (json_ctx*) ctx;
	if(c->key)
		free(c->key);
	c->key = (char*) malloc(len + 1);
	memcpy(c->key, s, len);
	c->key[len] = 0;
    return 1;
}

static int cb_start_map(void * ctx)
{
	json_ctx* c = (json_ctx*) ctx;
	json_value* v = to_apply(c);
	if(!v) return 0;
	v->type = JSON_TYPE_MAP;
	push(c, v);
    return 1;
}

static int cb_end_map(void * ctx)
{
	pop((json_ctx*) ctx);
    return 1;
}

static int cb_start_array(void * ctx)
{
	json_ctx* c = (json_ctx*) ctx;
	json_value* v = to_apply(c);
	if(!v) return 0;
	v->type = JSON_TYPE_ARRAY;
	push(c, v);
    return 1;
}

static int cb_end_array(void * ctx)
{
	pop((json_ctx*) ctx);
    return 1;
}

static yajl_callbacks callbacks = 
{
    cb_null,
    cb_boolean,
    cb_integer,
    cb_double,
    NULL,
    cb_string,
    cb_start_map,
    cb_map_key,
    cb_end_map,
    cb_start_array,
    cb_end_array
};

void* malloc_func(void *ctx, size_t sz)
{
	return malloc(sz);
}

void free_func(void *ctx, void * ptr)
{
	free(ptr);
}

void* realloc_func(void *ctx, void * ptr, size_t sz)
{
	return realloc(ptr, sz);
}

static yajl_alloc_funcs allocs =
{
	malloc_func,
	realloc_func,
	free_func
};

yajl_handle JSONCodec::AllocateHandle(json_ctx* ctx)
{
    return yajl_alloc(&callbacks, &allocs, (void *) ctx);
}

static char* typeStr = "type";
static char* boolStr = "bool";
static char* errStr = "error";
static char* intStr = "int";
static char* missingStr = "missing";
static char* nilStr = "nil";
static char* multiStr = "multi";
static char* numStr = "num";
static char* strStr = "str";
static char* rowsStr = "rows";
static char* colsStr = "cols";
static char* arrayStr = "array";
static char* colFirstStr = "colFirst";
static char* colLastStr = "colLast";
static char* rowFirstStr = "rowFirst";
static char* rowLastStr = "rowLast";

void JSONCodec::Encode(yajl_gen g, LPXLOPER x)
{
	yajl_gen_map_open(g);
	int type = x ? (x->xltype & ~(xlbitXLFree | xlbitDLLFree)) : xltypeNil;
	yajl_gen_string(g, (const unsigned char*) typeStr, strlen(typeStr));
	switch(type) {
		case xltypeBool:
			yajl_gen_integer(g, XL_CODEC_TYPE_BOOL);
			yajl_gen_string(g, (const unsigned char*) boolStr, strlen(boolStr));
			yajl_gen_bool(g, x->val.xbool);
			break;
		case xltypeErr:
			yajl_gen_integer(g, XL_CODEC_TYPE_ERR);
			yajl_gen_string(g, (const unsigned char*) errStr, strlen(errStr));
			yajl_gen_integer(g, x->val.err);
			break;
		case xltypeInt:
			yajl_gen_integer(g, XL_CODEC_TYPE_INT);
			yajl_gen_string(g, (const unsigned char*) intStr, strlen(intStr));
			yajl_gen_integer(g, x->val.w);
			break;
		case xltypeMissing:
			yajl_gen_integer(g, XL_CODEC_TYPE_MISSING);
			break;
		case xltypeNil:
			yajl_gen_integer(g, XL_CODEC_TYPE_NIL);
			break;
		case xltypeMulti:
			yajl_gen_integer(g, XL_CODEC_TYPE_MULTI);
			yajl_gen_string(g, (const unsigned char*) rowsStr, strlen(rowsStr));
			yajl_gen_integer(g, x->val.array.rows);
			yajl_gen_string(g, (const unsigned char*) colsStr, strlen(colsStr));
			yajl_gen_integer(g, x->val.array.columns);
			yajl_gen_string(g, (const unsigned char*) arrayStr, strlen(arrayStr));
			{
				int len = x->val.array.rows * x->val.array.columns;
				yajl_gen_array_open(g);
				for(int i = 0; i < len; i++) {
					Encode(g, &(x->val.array.lparray[i]));
				}
				yajl_gen_array_close(g);
			}
			break;
		case xltypeNum:
			yajl_gen_integer(g, XL_CODEC_TYPE_NUM);
			yajl_gen_string(g, (const unsigned char*) numStr, strlen(numStr));
			yajl_gen_double(g, x->val.num);
			break;
		case xltypeStr:
			yajl_gen_integer(g, XL_CODEC_TYPE_STR);
			yajl_gen_string(g, (const unsigned char*) strStr, strlen(strStr));
			yajl_gen_string(g, (const unsigned char*) &(x->val.str[1]), x->val.str[0]&0xff);
			break;
		case xltypeSRef:
			yajl_gen_integer(g, XL_CODEC_TYPE_SREF);
			yajl_gen_string(g, (const unsigned char*) colFirstStr, strlen(colFirstStr));
			yajl_gen_integer(g, x->val.sref.ref.colFirst);
			yajl_gen_string(g, (const unsigned char*) colLastStr, strlen(colLastStr));
			yajl_gen_integer(g, x->val.sref.ref.colLast);
			yajl_gen_string(g, (const unsigned char*) rowFirstStr, strlen(rowFirstStr));
			yajl_gen_integer(g, x->val.sref.ref.rwFirst);
			yajl_gen_string(g, (const unsigned char*) rowLastStr, strlen(rowLastStr));
			yajl_gen_integer(g, x->val.sref.ref.rwLast);
	}
	yajl_gen_map_close(g);
}


void JSONCodec::FreeJsonValue(json_value* v) 
{
	if(!v) return;
	switch(v->type) {
		case JSON_TYPE_MAP: {
			json_value* it = v->map.key;
			while(it) {
				json_value* t = it->map.key;
				FreeJsonValue(it);
				it = t;
			}
			it = v->map.value;
			while(it) {
				json_value* t = it->map.value;
				FreeJsonValue(it);
				it = t;
			}
		}
			break;
		case JSON_TYPE_ARRAY: {
			json_value* it = v->array.elem;
			while(it) {
				json_value* t = it->array.elem;
				FreeJsonValue(it);
				it = t;
			}
		}
		case JSON_TYPE_STR:
			free(v->str);
		break;
	}

	free(v);
}

bool JSONCodec::Decode(json_value* v, LPXLOPER x) 
{
	if(!v || v->type != JSON_TYPE_MAP) {
		return false;
	}

	json_value* t = get(v, "type");
	if(!t || t->type != JSON_TYPE_INT) {
		return false;
	}

	switch(t->i) {
		case XL_CODEC_TYPE_NUM: {
			x->xltype = xltypeNum;
			json_value* bv = get(v, "num");
			if(!bv) {
				return false;
			}
			if(bv->type == JSON_TYPE_INT) {
				x->val.num = bv->i;
			} else if(bv->type == JSON_TYPE_NUM) {
				x->val.num = bv->num;
			} else {
				x->val.num = 0;
			}
		}
		break;
		case XL_CODEC_TYPE_STR: {
			x->xltype = xltypeStr;
			json_value* bv = get(v, "str");
			if(!bv || bv->type != JSON_TYPE_STR) {
				return false;
			}
			int len = strlen(bv->str);
			if(len >255) {
				len=255;
			}
			x->val.str = (LPSTR) malloc(len + 1);
			x->val.str[0] = len;
			memcpy(&(x->val.str[1]), bv->str, len);
		}
		break;
		case XL_CODEC_TYPE_BOOL: {
			x->xltype = xltypeBool;
			json_value* bv = get(v, "bool");
			if(!bv || bv->type != JSON_TYPE_BOOL) {
				return false;
			}
			x->val.xbool = bv->b;
		}
		break;
		case XL_CODEC_TYPE_ERR: {
			x->xltype = xltypeErr;
			json_value* bv = get(v, "error");
			if(!bv || bv->type != JSON_TYPE_INT) {
				return false;
			}
			x->val.err = bv->i;
		}
		break;
		case XL_CODEC_TYPE_MULTI: {
			x->xltype = xltypeMulti;
			json_value* rowsv = get(v, "rows");
			if(!rowsv || rowsv->type != JSON_TYPE_INT) {
				return false;
			}
			x->val.array.rows = rowsv->i;
			json_value* colsv = get(v, "cols");
			if(!colsv || colsv->type != JSON_TYPE_INT) {
				return false;
			}
			x->val.array.columns = colsv->i;
			json_value* a = get(v, "array");
			if(!a || a->type != JSON_TYPE_ARRAY) {
				return false;
			}
			int len = x->val.array.rows * x->val.array.columns;
			if(len) {
				x->val.array.lparray = (LPXLOPER) malloc(sizeof(XLOPER) * len);
				json_value* it = a->array.elem;
				for(int i = 0; i < len; i++) {
					if(!it) 
						x->val.array.lparray[i].xltype = xltypeNil;
					else {
						if(!Decode(it, &x->val.array.lparray[i])) {
							return false;
						}
						it = it->array.elem;
					}
				}
			}
		}
		break;
		case XL_CODEC_TYPE_MISSING:
			x->xltype = xltypeMissing;
			break;
		case XL_CODEC_TYPE_NIL:
			x->xltype = xltypeNil;
			break;
		case XL_CODEC_TYPE_INT: {
			x->xltype = xltypeInt;
			json_value* bv = get(v, "int");
			if(!bv || bv->type != JSON_TYPE_INT) {
				free(x);
				return false;
			}
			x->val.w = bv->i;
		}
		break;
	}

	return true;
}
