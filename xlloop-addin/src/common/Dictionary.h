/*
Taken from http://ndevilla.free.fr/iniparser/

iniparser is a free stand-alone ini file parsing library.
It is written in portable ANSI C and should compile anywhere.
iniparser is distributed under an MIT license.

*/

#ifndef DICTIONARY_H
#define DICTIONARY_H

#include "Runtime.h"
#include <stdio.h>
#include <wctype.h>

typedef struct _dictionary_ {
	int				n ;		/** Number of entries in dictionary */
	int				size ;	/** Storage size */
	wchar_t 	**	val ;	/** List of string values */
	wchar_t 	**  key ;	/** List of string keys */
	unsigned	 *	hash ;	/** List of hash values for keys */
} dictionary ;

// Dictionary 

unsigned dictionary_hash(wchar_t * key);
dictionary * dictionary_new(int size);
void dictionary_del(dictionary * vd);
wchar_t * dictionary_get(dictionary * d, wchar_t * key, wchar_t * def);
wchar_t dictionary_getchar(dictionary * d, wchar_t * key, wchar_t def) ;
int dictionary_getint(dictionary * d, wchar_t * key, int def);
double dictionary_getdouble(dictionary * d, wchar_t * key, double def);
void dictionary_set(dictionary * vd, wchar_t * key, wchar_t * val);
void dictionary_unset(dictionary * d, wchar_t * key);
void dictionary_setint(dictionary * d, wchar_t * key, int val);
void dictionary_setdouble(dictionary * d, wchar_t * key, double val);
void dictionary_dump(dictionary * d, FILE * out);

// Ini parser

int iniparser_getnsec(dictionary * d);
wchar_t * iniparser_getsecname(dictionary * d, int n);
void iniparser_dump_ini(dictionary * d, FILE * f);
void iniparser_dump(dictionary * d, FILE * f);
wchar_t * iniparser_getstr(dictionary * d, const wchar_t * key);
wchar_t * iniparser_getstring(dictionary * d, const wchar_t * key, wchar_t * def);
int iniparser_getint(dictionary * d, const wchar_t * key, int notfound);
double iniparser_getdouble(dictionary * d, wchar_t * key, double notfound);
int iniparser_getboolean(dictionary * d, const wchar_t * key, int notfound);
int iniparser_setstr(dictionary * ini, wchar_t * entry, wchar_t * val);
void iniparser_unset(dictionary * ini, wchar_t * entry);
int iniparser_find_entry(dictionary * ini, wchar_t * entry) ;
dictionary * iniparser_load(wchar_t * ininame, bool isbuffer = false);
void iniparser_freedict(dictionary * d);

// Strlib 

wchar_t * strlwc(const wchar_t * s);
wchar_t * strupc(wchar_t * s);
wchar_t * strskp(wchar_t * s);
wchar_t * strcrop(wchar_t * s);
wchar_t * strstrip(wchar_t * s) ;
wchar_t * sgets(wchar_t* buffer, int* pos, wchar_t * line, int size) ;

#endif // DICTIONARY_H
