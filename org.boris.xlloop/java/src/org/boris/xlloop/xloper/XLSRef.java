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

public class XLSRef extends XLoper
{
    public final int colFirst;
    public final int colLast;
    public final int rwFirst;
    public final int rwLast;

    public XLSRef(int colFirst, int colLast, int rwFirst, int rwLast) {
        super(xlTypeSRef);
        this.colFirst = colFirst;
        this.colLast = colLast;
        this.rwFirst = rwFirst;
        this.rwLast = rwLast;
    }

    public static String toColumnName(int column) {
        if (column <= 0)
            return null;
        StringBuilder sb = new StringBuilder();
        while (column > 0) {
            int val = column % 26;
            if (val == 0)
                val = 26;
            column -= val;
            sb.insert(0, (char) ('A' + val - 1));
            column /= 26;
        }
        return sb.toString();
    }

    public boolean isArray() {
        return rwLast - rwFirst > 0 || colLast - colFirst > 0;
    }

    public String toString() {
        return toColumnName(colFirst) + rwFirst + (isArray() ? (":" + toColumnName(colLast) + rwLast) : "");
    }
}
