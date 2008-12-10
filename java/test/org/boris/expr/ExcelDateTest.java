/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr;

import java.util.Calendar;

import junit.framework.TestCase;

import org.boris.expr.util.ExcelDate;

public class ExcelDateTest extends TestCase
{
    public void test1() throws Exception {
        assertEquals(ExcelDate.toJavaDate(180.5), makeDate(1900, 6, 28, 12, 0,
                0, 0));
        assertEquals(ExcelDate.toJavaDate(14560.234), makeDate(1939, 11, 11, 5,
                36, 57, 600));
        assertEquals(ExcelDate.toJavaDate(36444.89), makeDate(1999, 10, 11, 21,
                21, 36, 0));
        assertEquals(ExcelDate.toJavaDate(10859.2492761586), makeDate(1929, 9,
                23, 5, 58, 57, 460));
        assertEquals(ExcelDate.toJavaDate(48433.439658647000), makeDate(2032,
                8, 7, 10, 33, 6, 507));
        assertEquals(ExcelDate.toJavaDate(87995.0647529107), makeDate(2140, 12,
                1, 1, 33, 14, 651));
    }

    public void test2() throws Exception {
        assertEquals(180.5, ExcelDate.toExcelDate(makeDate(1900, 6, 28, 12, 0,
                0, 0)));
        assertEquals(14560.234, ExcelDate.toExcelDate(makeDate(1939, 11, 11, 5,
                36, 57, 600)));
        assertEquals(36444.89, ExcelDate.toExcelDate(makeDate(1999, 10, 11, 21,
                21, 36, 0)));
        assertEquals(10859.2492761586, ExcelDate.toExcelDate(makeDate(1929, 9,
                23, 5, 58, 57, 460)));
        assertEquals(48433.439658647000, ExcelDate.toExcelDate(makeDate(2032,
                8, 7, 10, 33, 6, 507)));
        assertEquals(87995.0647529107, ExcelDate.toExcelDate(makeDate(2140, 12,
                1, 1, 33, 14, 651)));
    }

    public void test3() throws Exception {
        for (int i = 0; i < 100; i++) {
            double val = Math.random() * 100000;
            assertEquals(ExcelDate.toJavaDate(val), ExcelDate
                    .toJavaDate(ExcelDate
                            .toExcelDate(ExcelDate.toJavaDate(val))));
        }

    }

    private long makeDate(int year, int month, int day, int hour, int minute,
            int second, int millis) {
        Calendar c = Calendar.getInstance();
        c.set(year, month - 1, day, hour, minute, second);
        c.set(Calendar.MILLISECOND, millis);
        return c.getTimeInMillis();
    }

    static public boolean assertEquals(double val, double val2) {
        if (Math.abs(val - val2) > 0.0000001) {
            failNotSame(null, val, val2);
            return false;
        }
        return true;
    }
}
