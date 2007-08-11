/*******************************************************************************
* This program and the accompanying materials
* are made available under the terms of the Common Public License v1.0
* which accompanies this distribution, and is available at 
* http://www.eclipse.org/legal/cpl-v10.html
* 
* Contributors:
*     Peter Smith
*******************************************************************************/

#include "XLObjectFactory.h"
#include "XLObject.h"
#include "XLUtil.h"
#include "../common/Log.h"

bool XLObjectFactory::RegisterNatives(JNIEnv* env)
{
	JNINativeMethod natives[9];
	natives[0].fnPtr = XLObjectFactory::CreateArray;
	natives[0].name = "createArray";
	natives[0].signature = "(II)Lorg/excel4j/object/XLArray;";
	natives[1].fnPtr = XLObjectFactory::CreateBoolean;
	natives[1].name = "createBoolean";
	natives[1].signature = "(Z)Lorg/excel4j/object/XLBoolean;";
	natives[2].fnPtr = XLObjectFactory::CreateDouble;
	natives[2].name = "createDouble";
	natives[2].signature = "(D)Lorg/excel4j/object/XLNumber;";
	natives[3].fnPtr = XLObjectFactory::CreateError;
	natives[3].name = "createError";
	natives[3].signature = "(I)Lorg/excel4j/object/XLError;";
	natives[4].fnPtr = XLObjectFactory::CreateInteger;
	natives[4].name = "createInteger";
	natives[4].signature = "(I)Lorg/excel4j/object/XLInteger;";
	natives[5].fnPtr = XLObjectFactory::CreateReference1;
	natives[5].name = "createReference";
	natives[5].signature = "(II)Lorg/excel4j/object/XLReference;";
	natives[6].fnPtr = XLObjectFactory::CreateReference2;
	natives[6].name = "createReference";
	natives[6].signature = "(IIII)Lorg/excel4j/object/XLReference;";
	natives[7].fnPtr = XLObjectFactory::CreateReference3;
	natives[7].name = "createReference";
	natives[7].signature = "(IIIII)Lorg/excel4j/object/XLReference;";
	natives[8].fnPtr = XLObjectFactory::CreateString;
	natives[8].name = "createString";
	natives[8].signature = "(Ljava/lang/String;)Lorg/excel4j/object/XLString;";

	jclass clazz = env->FindClass("org/excel4j/XLObjectFactory");
	if(clazz == NULL) {
		Log::SetLastError("Could not find XLObjectFactory class.");
		return false;
	}

	jint ret = env->RegisterNatives(clazz, natives, 9);

	return true;
}

inline LPXLOPER CreateType(int xltype)
{
	LPXLOPER val = (LPXLOPER) malloc(sizeof(XLOPER));
	val->xltype = xltype;
	return val;
}

jobject XLObjectFactory::CreateArray(JNIEnv* env, jobject self, jint rows, jint columns)
{
	LPXLOPER val = CreateType(xltypeMulti);
	val->val.array.lparray = (LPXLOPER) malloc(sizeof(XLOPER) * rows * columns);
	val->val.array.columns = columns;
	val->val.array.rows = rows;

	return XLObject::CreateXLObject(env, val);;
}

jobject XLObjectFactory::CreateBoolean(JNIEnv* env, jobject self, jboolean value)
{
	LPXLOPER val = CreateType(xltypeBool);
	val->val.boolean = value;

	return XLObject::CreateXLObject(env, val);
}

jobject XLObjectFactory::CreateDouble(JNIEnv* env, jobject self, jdouble value)
{
	LPXLOPER val = CreateType(xltypeNum);
	val->val.num = value;

	return XLObject::CreateXLObject(env, val);
}

jobject XLObjectFactory::CreateError(JNIEnv* env, jobject self, jint value)
{
	LPXLOPER val = CreateType(xltypeErr);
	val->val.err = value;

	return XLObject::CreateXLObject(env, val);
}

jobject XLObjectFactory::CreateInteger(JNIEnv* env, jobject self, jint value)
{
	LPXLOPER val = CreateType(xltypeInt);
	val->val.w = value;

	return XLObject::CreateXLObject(env, val);
}

jobject XLObjectFactory::CreateReference1(JNIEnv* env, jobject self, jint row, jint column)
{
	LPXLOPER val = CreateType(xltypeRef);
	val->val.sref.count = 1;
	val->val.sref.ref.colFirst = column;
	val->val.sref.ref.colLast = column;
	val->val.sref.ref.rwFirst = row;
	val->val.sref.ref.rwLast = row;

	return XLObject::CreateXLObject(env, val);
}

jobject XLObjectFactory::CreateReference2(JNIEnv* env, jobject self, jint rowFirst, jint rowLast, jint colFirst, jint colLast)
{
	LPXLOPER val = CreateType(xltypeRef);
	val->val.sref.count = 1;
	val->val.sref.ref.colFirst = colFirst;
	val->val.sref.ref.colLast = colLast;
	val->val.sref.ref.rwFirst = rowFirst;
	val->val.sref.ref.rwLast = rowLast;

	return XLObject::CreateXLObject(env, val);
}

jobject XLObjectFactory::CreateReference3(JNIEnv* env, jobject self, jint sheetId, jint rowFirst, jint rowLast, jint colFirst, jint colLast)
{
	LPXLOPER val = CreateType(xltypeRef);
	val->val.mref.idSheet = sheetId;
	val->val.mref.lpmref = (LPXLMREF) malloc(sizeof(xlmref));

	return XLObject::CreateXLObject(env, val);
}

jobject XLObjectFactory::CreateString(JNIEnv* env, jobject self, jstring str)
{
	LPXLOPER val = CreateType(xltypeStr);
	jboolean iscopy = false;
	const char* chars = env->GetStringUTFChars(str, &iscopy);
	val->val.str = XLUtil::MakeExcelString(chars);
	env->ReleaseStringUTFChars(str, chars); 
	//val->xltype |= xlbitXLFree;

	return XLObject::CreateXLObject(env, val);
}

