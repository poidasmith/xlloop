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

    // Known constants
    public static final XLOper MISSING = makeType(XLOperType.xltypeMissing);
    public static final XLOper NULL = makeType(XLOperType.xltypeNil);
    public static final XLOper ERR_DIV0 = makeError(XLErrType.xlerrDiv0);
    public static final XLOper ERR_NA = makeError(XLErrType.xlerrNA);
    public static final XLOper ERR_NAME = makeError(XLErrType.xlerrName);
    public static final XLOper ERR_NULL = makeError(XLErrType.xlerrNull);
    public static final XLOper ERR_NUM = makeError(XLErrType.xlerrNum);
    public static final XLOper ERR_REF = makeError(XLErrType.xlerrRef);
    public static final XLOper ERR_VALUE = makeError(XLErrType.xlerrValue);

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

    public XLOper(int rows, int cols) {
        this.type = XLOperType.xltypeMulti;
        this.rows = rows;
        this.cols = cols;
        this.array = new XLOper[rows * cols];
    }

    public XLOper(XLOper[] arr) {
        this.type = XLOperType.xltypeMulti;
        this.rows = arr.length;
        this.cols = 1;
        this.array = arr;
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
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < array.length; i++) {
                if (i > 0)
                    sb.append(", ");
                if (array[i] == null)
                    sb.append(XLOper.NULL.toString());
                else
                    sb.append(array[i].toString());
            }
            sb.append("]");
            return sb.toString();
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

    public static XLOper makeError(int errNum) {
        XLOper err = new XLOper();
        err.type = XLOperType.xltypeErr;
        err.err = errNum;
        return err;
    }

    public static XLOper makeType(int type) {
        XLOper x = new XLOper();
        x.type = type;
        return x;
    }
}
