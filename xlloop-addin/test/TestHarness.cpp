
#include <stdio.h>
#include "../src/xll/BinaryProtocol.h"
#include "../src/xll/XLUtil.h"
#include "../src/common/Log.h"

int main()
{
	Log::Init(GetModuleHandle(NULL), NULL, L"debug", NULL);
	printf("testing\n");
	BinaryProtocol bp;
	bp.initialize(NULL, NULL);
	int cres = bp.connect();
	Log::Info(L"Checking result %d", cres);
	LPXLOPER12 res = bp.execute(L"test1", false, 0);
	XLUtil::LogFunctionCall(L"test.server", L"test1", res, 0);

}
