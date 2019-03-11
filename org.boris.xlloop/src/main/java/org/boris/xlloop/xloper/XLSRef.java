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
        StringBuffer sb = new StringBuffer();
        int t0 = column % 26;
        int t1 = column / 26;
        int t2 = column / 676;
        if (t2 > 0)
            sb.append((char) ('A' + t2 - 1));
        if (t1 > 0)
            sb.append((char) ('A' + t1 - 1));
        sb.append((char) ('A' + t0));
        return sb.toString();
    }

    public boolean isArray() {
        return rwLast - rwFirst > 0 || colLast - colFirst > 0;
    }

    public String toString() {
        return toColumnName(colFirst) + (rwFirst + 1) + (isArray() ? (":" + toColumnName(colLast) + (rwLast + 1)) : "");
    }
}
