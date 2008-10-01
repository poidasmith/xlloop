package org.boris.xlloop;

import org.boris.xlloop.xloper.XLList;
import org.boris.xlloop.xloper.XLMap;
import org.boris.xlloop.xloper.XLoper;

public class ExecuteHandler implements FunctionHandler
{
    public XLoper execute(String name, XLList args) throws RequestException {
        System.out.println(name + args);

        if (args.size() > 0) {
            return args.get(0);
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
