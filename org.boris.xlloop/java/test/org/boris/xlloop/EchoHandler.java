package org.boris.xlloop;

import org.boris.xlloop.util.XLList;
import org.boris.xlloop.xloper.XLoper;

public class EchoHandler implements FunctionHandler
{
    private int count;

    public XLoper execute(String name, XLoper[] args) throws RequestException {
        XLList l = new XLList(args);
        l.add(++count);
        return l.toXLoper();
    }

    public boolean hasFunction(String name) {
        return true;
    }
}
