using System;
using System.Collections.Generic;
using System.Text;

namespace XLLoop
{
    class ReflectFunctionHandler : FunctionHandler
    {
        public XLoper Execute(String name, XLoper[] args)
        {
            XLoper x = new XLoper();
            x.Type = XLoper.xlTypeNil;

            return x;
        }
    }
}
