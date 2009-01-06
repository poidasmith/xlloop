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

import java.util.Date;
import java.util.StringTokenizer;

import org.boris.expr.function.excel.DATE;
import org.boris.expr.function.excel.DATEVALUE;
import org.boris.expr.function.excel.DAY;
import org.boris.expr.function.excel.DAYS360;
import org.boris.expr.function.excel.HOUR;
import org.boris.expr.function.excel.MINUTE;
import org.boris.expr.function.excel.MONTH;
import org.boris.expr.function.excel.NOW;
import org.boris.expr.function.excel.SECOND;
import org.boris.expr.function.excel.TIME;
import org.boris.expr.function.excel.TIMEVALUE;
import org.boris.expr.function.excel.TODAY;
import org.boris.expr.function.excel.WEEKDAY;
import org.boris.expr.function.excel.YEAR;
import org.boris.expr.util.ExcelDate;
import org.boris.expr.util.IO;

public class ExcelDateAndTimeFunctionsTest extends TH
{

    public void testDATE() throws Exception {
        DATE d = new DATE();
        assertEquals(eval(d, 3, 5, 6), 1222.);
        assertEquals(eval(d, 3444, 65, 78), 565960.);
        assertEquals(eval(d, -12, -12, -12), ExprError.NUM);
        assertEquals(eval(d, 10000, 1, 1), ExprError.NUM);
    }

    public void testDATEVALUE() throws Exception {
        DATEVALUE d = new DATEVALUE();
        fail("DATEVALUE not implemented");
    }

    public void testDAY() throws Exception {
    }

    public void testDAYS360() throws Exception {
        DAYS360 d = new DAYS360();
        fail("DAYS360 not implemented");
    }

    public void testHOUR() throws Exception {

    }

    public void testMINUTE() throws Exception {

    }

    public void testMONTH() throws Exception {

    }

    public void testNOW() throws Exception {
        NOW n = new NOW();
        for (int i = 0; i < 100; i++) {
            assertEquals("NOW not working", eval(n), ExcelDate
                    .toExcelDate(new Date().getTime()));
        }
    }

    public void testSECOND() throws Exception {

    }

    public void testTIME() throws Exception {
        TIME t = new TIME();
        fail("TIME not implemented");
    }

    public void testTIMEVALUE() throws Exception {
        TIMEVALUE t = new TIMEVALUE();
        fail("TIMEVALUE not implemented");
    }

    public void testTODAY() throws Exception {
        TODAY t = new TODAY();
        for (int i = 0; i < 100; i++) {
            double d1 = (Double) eval(t);
            double d2 = ExcelDate.toExcelDate(new Date().getTime());
            assertTrue("TODAY not working (" + d1 + "," + d2 + ")", Math
                    .abs(d1 - d2) < 0.000001);
        }
    }

    public void testWEEKDAY() throws Exception {
    }

    public void testYEAR() throws Exception {
    }

    /** ************************************** */

    public void test() throws Exception {
        for (String line : IO.readLines(getClass(), "date_test_data.txt")) {
            String[] vals = IO.toArray(new StringTokenizer(line, " \t"));
            testDates(d(vals[0]), i(vals[1]), i(vals[2]), i(vals[3]),
                    i(vals[4]), i(vals[5]), i(vals[6]), i(vals[7]));
        }
    }

    private double d(String d) {
        return Double.parseDouble(d);
    }

    private int i(String s) {
        return Integer.parseInt(s);
    }

    private void testDates(double date, int day, int month, int year, int hour,
            int minute, int second, int weekday) throws ExprException {
        assertEquals("testing " + date, eval(new DAY(), date), (double) day);
        assertEquals("testing " + date, eval(new MONTH(), date), (double) month);
        assertEquals("testing " + date, eval(new YEAR(), date), (double) year);
        assertEquals("testing " + date, eval(new HOUR(), date), (double) hour);
        assertEquals("testing " + date, eval(new MINUTE(), date),
                (double) minute);
        assertEquals("testing " + date, eval(new SECOND(), date),
                (double) second);
        assertEquals("testing " + date, eval(new WEEKDAY(), date),
                (double) weekday);
    }
}
