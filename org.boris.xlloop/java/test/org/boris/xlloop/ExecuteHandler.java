package org.boris.xlloop;

import org.boris.xlloop.util.XLMap;
import org.boris.xlloop.xloper.XLoper;

public class ExecuteHandler implements IFunctionHandler
{
    public XLoper execute(IFunctionContext context, String name, XLoper[] args) throws RequestException {
        System.out.println(name + args);

        if (args.length > 0) {
            return args[0];
        }

        XLMap s = new XLMap();
        s.add("hello there", 123213);
        s.add("asdf", "woeiruewoir");
        return s.toXloper();
    }

    public boolean hasFunction(String name) {
        return true;
    }
}
