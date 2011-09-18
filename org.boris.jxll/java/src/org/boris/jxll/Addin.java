/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.jxll;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Addin
{
    long library;
    String name;

    private List functions = new ArrayList();
    private Map information = new HashMap();

    Addin(long library, String name) {
        this.library = library;
        this.name = name;
    }

    public XLOper invoke(String name) {
        return invoke(name, new Object[0]);
    }

    public XLOper invoke(String name, Object a1) {
        return invoke(name, new Object[] { a1 });
    }

    public XLOper invoke(String name, Object a1, Object a2) {
        return invoke(name, new Object[] { a1, a2 });
    }

    public XLOper invoke(String name, Object a1, Object a2, Object a3) {
        return invoke(name, new Object[] { a1, a2, a3 });
    }

    public XLOper invoke(String name, Object a1, Object a2, Object a3, Object a4) {
        return invoke(name, new Object[] { a1, a2, a3, a4 });
    }

    public XLOper invoke(String name, Object a1, Object a2, Object a3, Object a4, Object a5) {
        return invoke(name, new Object[] { a1, a2, a3, a4, a5 });
    }

    public XLOper invoke(String name, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
        return invoke(name, new Object[] { a1, a2, a3, a4, a5, a6 });
    }

    public XLOper invoke(String name, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
        return invoke(name, new Object[] { a1, a2, a3, a4, a5, a6, a7 });
    }

    public XLOper invoke(String name, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7,
            Object a8) {
        return invoke(name, new Object[] { a1, a2, a3, a4, a5, a6, a7, a8 });
    }

    public XLOper invoke(String name, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7,
            Object a8, Object a9) {
        return invoke(name, new Object[] { a1, a2, a3, a4, a5, a6, a7, a8, a9 });
    }

    public XLOper invoke(String name, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7,
            Object a8, Object a9, Object a10) {
        return invoke(name, new Object[] { a1, a2, a3, a4, a5, a6, a7, a8, a9, a10 });
    }

    public XLOper invoke(String name, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7,
            Object a8, Object a9, Object a10, Object a11) {
        return invoke(name, new Object[] { a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11 });
    }

    public XLOper invoke(String name, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7,
            Object a8, Object a9, Object a10, Object a11, Object a12) {
        return invoke(name, new Object[] { a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12 });
    }

    public XLOper invoke(String name, Object[] args) {
        XLOper[] a = new XLOper[args.length];
        for (int i = 0; i < a.length; i++) {
            a[i] = convert(args[i]);
        }
        return invoke(name, a);
    }

    public XLOper invoke(String name, XLOper[] args) {
        FunctionInformation fi = (FunctionInformation) information.get(name);
        if (fi == null)
            return null;
        if (args.length != fi.type.types.length)
            args = normalizeArgs(args, fi.type);
        return JNI.invoke(library, fi.procedure, fi.type.returnType, fi.type.types, args);
    }

    public XLOper invoke(String name, XLOper[] args, boolean normalizeArgs) {
        FunctionInformation fi = (FunctionInformation) information.get(name);
        if (fi == null)
            return null;
        if (args.length != fi.type.types.length)
            if (normalizeArgs)
                args = normalizeArgs(args, fi.type);
            else
                return null;
        return JNI.invoke(library, fi.procedure, fi.type.returnType, fi.type.types, args);
    }

    public static XLOper[] normalizeArgs(XLOper[] args, FunctionSpec spec) {
        XLOper[] na = new XLOper[spec.types.length];
        System.arraycopy(args, 0, na, 0, Math.min(args.length, na.length));
        if (na.length > args.length) {
            for (int i = args.length; i < na.length; i++) {
                na[i] = XLOper.MISSING;
            }
        }
        return na;
    }

    public static XLOper convert(Object arg) {
        if (arg instanceof Boolean) {
            return new XLOper(((Boolean) arg).booleanValue());
        } else if (arg instanceof Number) {
            return new XLOper(((Number) arg).doubleValue());
        } else if (arg instanceof String) {
            return new XLOper((String) arg);
        } else if (arg instanceof XLOper) {
            return (XLOper) arg;
        }
        return null;
    }

    public int getFunctionCount() {
        return functions.size();
    }

    public String getFunctionName(int index) {
        return (String) functions.get(index);
    }

    public FunctionInformation getInformation(String name) {
        return (FunctionInformation) information.get(name);
    }

    public boolean hasFunction(String name) {
        return functions.contains(name);
    }

    public void registerFunction(FunctionInformation fi) {
        if (fi == null || fi.functionName == null || fi.procedure == null)
            return;
        if (fi.type == null)
            fi.type = FunctionSpec.valueOf(fi.typeText);
        if (fi.type == null)
            return;

        functions.add(fi.functionName);
        information.put(fi.functionName, fi);
    }

    void registerFunction(XLOper[] opers) {
        FunctionInformation fi = new FunctionInformation();
        fi.procedure = opers[1].str;
        if (opers.length > 2 && opers[2] != null)
            fi.typeText = opers[2].str;
        if (opers.length > 3 && opers[3] != null)
            fi.functionName = opers[3].str;
        if (opers.length > 4 && opers[4] != null)
            fi.argumentText = opers[4].str;
        if (opers.length > 5 && opers[5] != null)
            fi.macroType = opers[5].str;
        if (opers.length > 6 && opers[6] != null)
            fi.category = opers[6].str;
        if (opers.length > 7 && opers[7] != null)
            fi.shortcutText = opers[7].str;
        if (opers.length > 8 && opers[8] != null)
            fi.helpTopic = opers[8].str;
        if (opers.length > 9 && opers[9] != null)
            fi.functionHelp = opers[9].str;
        if (opers.length > 10) {
            fi.argumentHelp = new String[opers.length - 10];
            for (int i = 0; i < fi.argumentHelp.length; i++) {
                if (opers[i + 10] != null)
                    fi.argumentHelp[i] = opers[i + 10].str;
            }
        }
        fi.type = FunctionSpec.valueOf(fi.typeText);
        functions.add(fi.functionName);
        information.put(fi.functionName, fi);
    }
}
