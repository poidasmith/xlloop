/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.boris.xlloop.xloper.XLArray;
import org.boris.xlloop.xloper.XLBool;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLNil;
import org.boris.xlloop.xloper.XLNum;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

public class XLSparseArray
{
    private int minRow = Integer.MAX_VALUE;
    private int minCol = Integer.MAX_VALUE;
    private int maxRow = 0;
    private int maxCol = 0;
    private Map<ArrayRef, XLoper> values = new HashMap();

    public int columns() {
        return maxCol;
    }

    public int rows() {
        return maxRow;
    }

    public void set(int row, int column, String value) {
        set(row, column, new XLString(value));
    }

    public void set(int row, int column, boolean value) {
        set(row, column, value ? XLBool.TRUE : XLBool.FALSE);
    }

    public void set(int row, int column, double value) {
        set(row, column, new XLNum(value));
    }

    public void set(int row, int column, int value) {
        set(row, column, new XLInt(value));
    }

    public void set(int row, int column, XLoper value) {
        if (value == null)
            value = XLNil.NIL;
        if (minRow > row)
            minRow = row;
        if (maxRow < row)
            maxRow = row;
        if (minCol > column)
            minCol = column;
        if (maxCol < column)
            maxCol = column;
        values.put(new ArrayRef(row, column), value);
    }

    public String getString(int row, int column) {
        XLoper x = get(row, column);
        if (x instanceof XLString)
            return ((XLString) x).str;
        return null;
    }

    public Double getDouble(int row, int column) {
        XLoper x = get(row, column);
        if (x instanceof XLNum)
            return ((XLNum) x).num;
        else if (x instanceof XLInt)
            return (double) ((XLInt) x).w;
        else if (x instanceof XLBool)
            return ((XLBool) x).bool ? 1. : 0.;
        return null;
    }

    public XLoper get(int row, int column) {
        return values.get(new ArrayRef(row, column));
    }

    public XLoper toXLoper() {
        XLArray a = new XLArray(maxRow - minRow + 1, maxCol - minCol + 1);
        for (Iterator i = values.keySet().iterator(); i.hasNext();) {
            ArrayRef r = (ArrayRef) i.next();
            a.set(r.row - minRow, r.column - minCol, (XLoper) values.get(r));
        }
        return a;
    }

    public XLArray toArray() {
        return (XLArray) toXLoper();
    }

    private class ArrayRef
    {
        public ArrayRef(int row, int column) {
            this.row = row;
            this.column = column;
        }

        public int row;
        public int column;

        public boolean equals(Object obj) {
            return ((ArrayRef) obj).row == row && ((ArrayRef) obj).column == column;
        }

        public int hashCode() {
            return row ^ column;
        }
    }
}
