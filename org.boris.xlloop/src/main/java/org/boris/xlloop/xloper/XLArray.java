/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.xloper;

public class XLArray extends XLoper
{
    public final int rows;
    public final int columns;
    public final int length;
    public final XLoper[] array;

    public XLArray(XLoper[] array, int rows, int columns) {
        super(xlTypeMulti);
        this.rows = rows;
        this.columns = columns;
        this.length = rows * columns;
        this.array = array;
    }

    public XLArray(int rows, int columns) {
        super(xlTypeMulti);
        this.rows = rows;
        this.columns = columns;
        this.length = rows * columns;
        this.array = new XLoper[rows * columns];
    }

    public void set(int row, int column, XLoper value) {
        array[row * columns + column] = value;
    }

    public void set(int row, int column, String value) {
        set(row, column, new XLString(value));
    }

    public void set(int row, int column, int value) {
        set(row, column, new XLInt(value));
    }

    public void set(int row, int column, Integer value) {
        set(row, column, value == null ? (XLoper) XLNil.NIL : new XLInt(value
                .intValue()));
    }

    public void set(int row, int column, double value) {
        set(row, column, new XLNum(value));
    }

    public void set(int row, int column, Double value) {
        set(row, column, value == null ? (XLoper) XLNil.NIL : new XLNum(value
                .doubleValue()));
    }

    public void set(int row, int column, boolean value) {
        set(row, column, new XLBool(value));
    }

    public void set(int row, int column, Boolean value) {
        set(row, column, value == null ? (XLoper) XLNil.NIL : new XLBool(value
                .booleanValue()));
    }

    public XLoper get(int index) {
        return array[index];
    }

    public XLoper get(int row, int column) {
        return array[row * columns + column];
    }

    public String getString(int index) {
        XLoper xl = array[index];
        if (xl.type == xlTypeStr) {
            return ((XLString) xl).str;
        }
        return null;
    }

    public Double getDouble(int index) {
        XLoper xl = array[index];
        if (xl.type == xlTypeNum) {
            return new Double(((XLNum) xl).num);
        } else if (xl.type == xlTypeInt) {
            return new Double(((XLInt) xl).w);
        } else if (xl.type == xlTypeBool) {
            return new Double(((XLBool) xl).bool ? 1 : 0);
        }
        return null;
    }

    public Integer getInteger(int index) {
        XLoper xl = array[index];
        if (xl.type == xlTypeNum) {
            return new Integer((int) ((XLNum) xl).num);
        } else if (xl.type == xlTypeInt) {
            return new Integer((int) ((XLInt) xl).w);
        } else if (xl.type == xlTypeBool) {
            return new Integer(((XLBool) xl).bool ? 1 : 0);
        }
        return null;
    }

    public Boolean getBoolean(int index) {
        XLoper xl = array[index];
        if (xl == null)
            return null;
        if (xl.type == xlTypeBool) {
            return new Boolean(((XLBool) xl).bool);
        } else if (xl.type == xlTypeNum) {
            return new Boolean(((int) ((XLNum) xl).num) != 0);
        } else if (xl.type == xlTypeInt) {
            return new Boolean(((XLInt) xl).w != 0);
        }
        return null;
    }

    public Double getDouble(int row, int column) {
        return getDouble(row * columns + column);
    }

    public Integer getInteger(int row, int column) {
        return getInteger(row * columns + column);
    }

    public Boolean getBoolean(int row, int column) {
        return getBoolean(row * columns + column);
    }

    public String getString(int row, int column) {
        return getString(row * columns + column);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < rows; i++) {
            sb.append("[");
            for (int j = 0; j < columns; j++) {
                if (j > 0)
                    sb.append(",");
                sb.append(array[(i * columns) + j]);
            }
            sb.append("]");
            if (i < rows - 1)
                sb.append("\n");
        }
        return sb.toString();
    }
}
