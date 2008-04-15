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
import java.util.Collection;
import java.util.Iterator;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTDouble;
import org.boris.variantcodec.VTLong;
import org.boris.variantcodec.VTNull;
import org.boris.variantcodec.VTString;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.FunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.handler.FunctionInformation;
import org.boris.xlloop.util.IO;
import org.boris.xlloop.util.ObjectRegistry;
import org.jatha.Jatha;
import org.jatha.dynatype.LispConsOrNil;
import org.jatha.dynatype.LispValue;
import org.jatha.dynatype.StandardLispCons;
import org.jatha.dynatype.StandardLispHashTable;
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

    public Variant execute(String name, VTCollection args) throws RequestException {
        try {
            LispValue inValue = makeList(args, findSize(args));
            LispValue result = jatha.eval(inValue);
            Variant outValue = makeResult(result);
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
        fi.setFunctionHelp("Evaluates a list of arguments as a lisp expression");
        return fi;
    }
    
    /**
     * Eval scripts in a directory tree.
     * 
     * @param f
     * @param recurse
     */
    public void eval(File f, boolean recurse) {
        if(f == null) return;
        if(f.isDirectory() && recurse) {
            File[] fs = f.listFiles();
            for(int i = 0; i < fs.length; i++) {
                eval(fs[i], recurse);
            }
        } else if(f.isFile() && f.getName().endsWith(".lisp")) {
            try {
                String s = IO.toString(f);
                jatha.eval(s);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private LispValue makeList(VTCollection args, int size) throws EOFException {
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

    private LispValue makeValue(Variant value) throws EOFException {
        if (value instanceof VTString) {
            String str = ((VTString) value).get();
            LispValue l = (LispValue) registry.get(str);
            if (l != null) {
                return l;
            } else {
                return jatha.parse(str);
            }
        } else if (value instanceof VTLong) {
            return jatha.makeInteger(((VTLong) value).longValue());
        } else if (value instanceof VTDouble) {
            return jatha.makeReal(((VTDouble) value).doubleValue());
        } else if (value instanceof VTCollection) {
            return makeList((VTCollection) value, findSize((VTCollection) value));
        } else if (value instanceof VTStruct) {
            VTStruct s = (VTStruct) value;
            StandardLispHashTable t = new StandardLispHashTable(jatha);
            for (Iterator i = s.getKeys().iterator(); i.hasNext(); ) {
                String str = (String) i.next();
                t.setf_gethash(new StandardLispString(jatha, str), makeValue(s.getValue(str)));
            }
            return t;
        } else {
            return new StandardLispNIL();
        }
    }

    private Variant makeResult(LispValue value) {
        if (value instanceof StandardLispString) {
            return new VTString(((StandardLispString) value).getValue());
        } else if (value instanceof StandardLispInteger) {
            return new VTLong(((StandardLispInteger) value).getValue());
        } else if (value instanceof StandardLispReal) {
            return new VTDouble(((StandardLispReal) value).getDoubleValue());
        } else if (value instanceof StandardLispCons) {
            StandardLispCons c = (StandardLispCons) value;
            VTCollection coll = new VTCollection();
            for (int i = 0; i < c.basic_length(); i++) {
                coll.add(makeResult(c.elt(i)));
            }
            return coll;
        } else if (value instanceof StandardLispHashTable) {
            Collection c = ((StandardLispHashTable) value).toCollection();
            VTCollection coll = new VTCollection();
            for (Iterator i = c.iterator(); i.hasNext();) {
                coll.add(makeResult((LispValue) i.next()));
            }
            return coll;
        } else if (value instanceof StandardLispNIL || value == null) {
            return VTNull.NULL;
        } else {
            return new VTString(registry.put(value));
        }
    }

    private int findSize(VTCollection args) {
        int size;
        if (args == null || (size = args.size()) == 0) {
            return 0;
        }
        while (size > 0) {
            Variant v = args.get(--size);
            if (!(v instanceof VTNull)) {
                return size + 1;
            }
        }
        return 0;
    }
}
