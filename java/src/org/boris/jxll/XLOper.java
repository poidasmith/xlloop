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

public class XLOper
{
    public int type;
    public double num;
    public String str;
    public boolean bool;
    public int err;
    public int w;
    public int rows;
    public int cols;
    public XLOper[] array;
    public XLRef ref;
    public XLRef[] mref;
    public int idSheet;

    public static final XLOper MISSING = new XLOper();
    public static final XLOper NULL = new XLOper();
    static {
        MISSING.type = XLOperType.xltypeMissing;
        NULL.type = XLOperType.xltypeNil;
    }

    public XLOper() {
    }

    public XLOper(String s) {
        type = XLOperType.xltypeStr;
        str = s;
    }

    public XLOper(double d) {
        type = XLOperType.xltypeNum;
        num = d;
    }

    public XLOper(int i) {
        type = XLOperType.xltypeNum;
        num = i;
    }

    public XLOper(boolean b) {
        type = XLOperType.xltypeBool;
        bool = b;
    }

    public String toString() {
        switch (type) {
        case XLOperType.xltypeBool:
            return Boolean.toString(bool);
        case XLOperType.xltypeErr: // TODO err string
            return Integer.toString(err);
        case XLOperType.xltypeInt:
            return Integer.toString(w);
        case XLOperType.xltypeMissing:
            return "#(missing)";
        case XLOperType.xltypeMulti: {
        }
        case XLOperType.xltypeNil:
            return "#(null)";
        case XLOperType.xltypeNum:
            return Double.toString(num);
        case XLOperType.xltypeRef:
        case XLOperType.xltypeSRef:
            return "{" + ref.colFirst + "," + ref.colLast + "," + ref.rwFirst + "," + ref.rwLast + "}";
        case XLOperType.xltypeStr:
            return str;
        }

        return null;
    }
}
