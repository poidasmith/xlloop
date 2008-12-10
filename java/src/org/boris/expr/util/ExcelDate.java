/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr.util;

import java.util.Calendar;

public class ExcelDate
{
    private static final double MS_IN_DAY = 86400000; // 1/(24*60*60*1000)
    private static final long EPOCH_OFFSET = 2209182800000l;

    public static long toJavaDate(double value) {
        return Math.round(value * MS_IN_DAY - EPOCH_OFFSET);
    }

    public static long toJavaDate2(double value) {
        Calendar c = Calendar.getInstance();
        c.setLenient(true);
        c.set(1900, 0, 0, 0, 0, 0);
        c.set(Calendar.DAY_OF_YEAR, (int) value - 1);
        c.set(Calendar.MILLISECOND, (int) Math.round((value % 1) * MS_IN_DAY));
        return c.getTimeInMillis();
    }

    public static double toExcelDate(long millis) {
        return ((double) millis + EPOCH_OFFSET) / MS_IN_DAY;
    }
}
