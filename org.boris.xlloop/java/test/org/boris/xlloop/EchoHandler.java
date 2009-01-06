package org.boris.xlloop;

import org.boris.xlloop.util.XLList;
import org.boris.xlloop.xloper.XLoper;

public class EchoHandler implements FunctionHandler
{
    private int count;

    public XLoper execute(String name, XLList args) throws RequestException {
        args.add(++count);
        return args.toXLoper();
    }

    public boolean hasFunction(String name) {
        return true;
    }
}
