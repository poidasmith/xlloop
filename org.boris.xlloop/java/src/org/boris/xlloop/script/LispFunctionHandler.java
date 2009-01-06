/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.script;

import java.io.EOFException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.boris.xlloop.FunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.handler.FunctionInformation;
import org.boris.xlloop.util.IO;
import org.boris.xlloop.util.ObjectRegistry;
import org.boris.xlloop.util.XLList;
import org.boris.xlloop.xloper.XLArray;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLMissing;
import org.boris.xlloop.xloper.XLNil;
import org.boris.xlloop.xloper.XLNum;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;
import org.jatha.Jatha;
import org.jatha.dynatype.LispConsOrNil;
import org.jatha.dynatype.LispValue;
import org.jatha.dynatype.StandardLispCons;
import org.jatha.dynatype.StandardLispInteger;
import org.jatha.dynatype.StandardLispNIL;
import org.jatha.dynatype.StandardLispReal;
import org.jatha.dynatype.StandardLispString;
import org.jatha.dynatype.StandardLispSymbol;

public class LispFunctionHandler implements FunctionHandler
{
    private Jatha jatha = new Jatha(false, false, false);
    private ObjectRegistry registry = new ObjectRegistry();

    public LispFunctionHandler() {
        jatha.init();
        jatha.start();
    }

    public XLoper execute(String name, XLList args) throws RequestException {
        try {
            LispValue inValue = makeList(args, findSize(args));
            LispValue result = jatha.eval(inValue);
            XLoper outValue = makeResult(result);
            return outValue;
        } catch (Exception e) {
            e.printStackTrace();
            throw new RequestException(e);
        }
    }

    public boolean hasFunction(String name) {
        return name.equals("Eval");
    }

    public FunctionInformation getInformation() {
        FunctionInformation fi = new FunctionInformation("Eval");
        fi.addArgument("args", "The arguments...");
        fi
                .setFunctionHelp("Evaluates a list of arguments as a lisp expression");
        return fi;
    }

    /**
     * Eval scripts in a directory tree.
     * 
     * @param f
     * @param recurse
     */
    public void eval(File f, boolean recurse) {
        if (f == null)
            return;
        if (f.isDirectory() && recurse) {
            File[] fs = f.listFiles();
            for (int i = 0; i < fs.length; i++) {
                eval(fs[i], recurse);
            }
        } else if (f.isFile() && f.getName().endsWith(".lisp")) {
            try {
                String s = IO.toString(f);
                jatha.eval(s);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private LispValue makeList(XLList args, int size) throws EOFException {
        if (args == null || size == 0) {
            return new StandardLispNIL();
        } else if (size == 1) {
            return makeValue(args.get(0));
        } else {
            ArrayList l = new ArrayList();
            boolean quote = false;
            for (int i = 0; i < size; i++) {
                LispValue v = makeValue(args.get(i));
                if (v != null)
                    l.add(v);
                if (i == 0 && !(v instanceof StandardLispSymbol)) {
                    quote = true;
                }
            }
            LispConsOrNil res = jatha.makeList(l);
            if (quote)
                res = jatha.makeList(jatha.QUOTE, res);
            return res;
        }
    }

    private LispValue makeValue(XLoper value) throws EOFException {
        if (value instanceof XLString) {
            String str = ((XLString) value).str;
            LispValue l = (LispValue) registry.get(str);
            if (l != null) {
                return l;
            } else {
                return jatha.parse(str);
            }
        } else if (value instanceof XLInt) {
            return jatha.makeInteger(((XLInt) value).w);
        } else if (value instanceof XLNum) {
            return jatha.makeReal(((XLNum) value).num);
        } else if (value instanceof XLArray) {
            XLList l = new XLList((XLArray) value);
            return makeList(l, findSize(l));
        } else {
            return new StandardLispNIL();
        }
    }

    private XLoper makeResult(LispValue value) {
        if (value instanceof StandardLispString) {
            return new XLString(((StandardLispString) value).getValue());
        } else if (value instanceof StandardLispInteger) {
            return new XLInt((int) ((StandardLispInteger) value).getValue());
        } else if (value instanceof StandardLispReal) {
            return new XLNum(((StandardLispReal) value).getDoubleValue());
        } else if (value instanceof StandardLispCons) {
            StandardLispCons c = (StandardLispCons) value;
            XLList coll = new XLList();
            for (int i = 0; i < c.basic_length(); i++) {
                coll.add(makeResult(c.elt(i)));
            }
            return coll.toXLoper();
        } else if (value instanceof StandardLispNIL || value == null) {
            return XLNil.NIL;
        } else {
            return new XLString(registry.put(value));
        }
    }

    private int findSize(XLList args) {
        int size;
        if (args == null || (size = args.size()) == 0) {
            return 0;
        }
        while (size > 0) {
            XLoper v = args.get(--size);
            if (!(v instanceof XLMissing)) {
                return size + 1;
            }
        }
        return 0;
    }
}
