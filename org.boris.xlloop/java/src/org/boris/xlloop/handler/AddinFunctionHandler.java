/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.handler;

import org.boris.jxll.Addin;
import org.boris.jxll.XLOper;
import org.boris.jxll.XLOperType;
import org.boris.xlloop.FunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.util.CSV;
import org.boris.xlloop.xloper.XLArray;
import org.boris.xlloop.xloper.XLBool;
import org.boris.xlloop.xloper.XLError;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLMissing;
import org.boris.xlloop.xloper.XLNil;
import org.boris.xlloop.xloper.XLNum;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

public class AddinFunctionHandler implements FunctionHandler, FunctionProvider
{
    private Addin addin;
    private FunctionInformation[] info;

    public AddinFunctionHandler(Addin addin) {
        this.addin = addin;
        info = new FunctionInformation[addin.getFunctionCount()];
        for (int i = 0; i < info.length; i++) {
            String n = addin.getFunctionName(i);
            org.boris.jxll.FunctionInformation fi = addin.getInformation(n);
            info[i] = new FunctionInformation(n);
            info[i].setCategory(fi.category);
            info[i].setFunctionHelp(fi.functionHelp);
            info[i].setHelpTopic(fi.helpTopic);
            info[i].setShortcutText(fi.shortcutText);
            String[] args = new String[0];
            if (fi.argumentText != null)
                CSV.parseLine(fi.argumentText, ',', false);
            String[] ahelp = fi.argumentHelp;
            for (int j = 0; j < args.length; j++) {
                String arg = args[j];
                String ah = "";
                if (ahelp != null && ahelp.length > j)
                    ah = ahelp[j];
                info[i].addArgument(arg, ah);
            }
            if (fi.type != null) {
                info[i].setVolatile(fi.type.isVolatile);
            }
        }
    }

    public XLoper execute(String name, XLoper[] args) throws RequestException {
        org.boris.jxll.FunctionInformation fi = addin.getInformation(name);
        if (fi == null || fi.type == null) {
            return null;
        }
        XLOper[] a = new XLOper[fi.type.types.length];
        for (int i = 0; i < a.length; i++) {
            if (i < args.length) {
                a[i] = convert(args[i]);
            } else {
                a[i] = new XLOper();
                a[i].type = XLOperType.xltypeMissing;
            }
        }
        XLOper x = addin.invoke(name, a);
        XLoper r = convert(x);
        return r;
    }

    public boolean hasFunction(String name) {
        return addin.hasFunction(name);
    }

    public FunctionInformation[] getFunctions() {
        return info;
    }

    private XLOper convert(XLoper x) {
        XLOper o = new XLOper();
        if (x instanceof XLArray) {
            o.type = XLOperType.xltypeMulti;
            XLArray a = (XLArray) x;
            o.rows = a.rows;
            o.cols = a.columns;
            o.array = new XLOper[a.array.length];
            for (int i = 0; i < o.array.length; i++) {
                o.array[i] = convert(a.array[i]);
            }
        } else if (x instanceof XLBool) {
            o.type = XLOperType.xltypeBool;
            o.bool = ((XLBool) x).bool;
        } else if (x instanceof XLError) {
            o.type = XLOperType.xltypeErr;
            o.err = ((XLError) x).err;
        } else if (x instanceof XLInt) {
            o.type = XLOperType.xltypeInt;
            o.w = ((XLInt) x).w;
        } else if (x instanceof XLMissing) {
            o.type = XLOperType.xltypeMissing;
        } else if (x instanceof XLNil) {
            o.type = XLOperType.xltypeNil;
        } else if (x instanceof XLNum) {
            o.type = XLOperType.xltypeNum;
            o.num = ((XLNum) x).num;
        } else if (x instanceof XLString) {
            o.type = XLOperType.xltypeStr;
            o.str = ((XLString) x).str;
        }
        return o;
    }

    private XLoper convert(XLOper x) {
        switch (x.type) {
        case XLOperType.xltypeBool:
            return new XLBool(x.bool);
        case XLOperType.xltypeErr:
            return new XLError(x.err);
        case XLOperType.xltypeInt:
            return new XLInt(x.w);
        case XLOperType.xltypeMissing:
            return XLMissing.MISSING;
        case XLOperType.xltypeMulti:
            XLArray a = new XLArray(x.rows, x.cols);
            for (int i = 0; i < x.array.length; i++) {
                a.array[i] = convert(x.array[i]);
            }
            return a;
        case XLOperType.xltypeNil:
            return XLNil.NIL;
        case XLOperType.xltypeNum:
            return new XLNum(x.num);
        case XLOperType.xltypeStr:
            return new XLString(x.str);
        }
        return null;
    }
}
