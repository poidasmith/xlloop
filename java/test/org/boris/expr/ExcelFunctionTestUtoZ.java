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

import org.boris.expr.function.excel.UPPER;
import org.boris.expr.function.excel.VALUE;
import org.boris.expr.function.excel.VARA;
import org.boris.expr.function.excel.VARPA;
import org.boris.expr.function.excel.VDB;
import org.boris.expr.function.excel.VLOOKUP;
import org.boris.expr.function.excel.WEEKDAY;
import org.boris.expr.function.excel.WEIBULL;
import org.boris.expr.function.excel.YEAR;
import org.boris.expr.function.excel.ZTEST;

public class ExcelFunctionTestUtoZ extends TH
{
    public void testUPPER() throws Exception {
        UPPER u = new UPPER();
        assertEquals(eval(u, "asdf"), "ASDF");
        assertEquals(eval(u, true), "TRUE");
        assertEquals(eval(u, 1), "1");
        assertException("upper(1,1)");
        assertResult("upper({1,2})", ExprError.VALUE);
    }

    public void testVALUE() throws Exception {
        VALUE v = new VALUE();
        fail("VALUE not implemented");
    }

    public void testVAR() throws Exception {
        assertResult(
                "var({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                754.2666666665);
    }

    public void testVARA() throws Exception {
        VARA v = new VARA();
        fail("VARA not implemented");
    }

    public void testVARP() throws Exception {
        assertResult(
                "varp({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                754.2666666665);
    }

    public void testVARPA() throws Exception {
        VARPA v = new VARPA();
        fail("VARPA not implemented");
    }

    public void testVDB() throws Exception {
        VDB v = new VDB();
        fail("VDB not implemented");
    }

    public void testVLOOKUP() throws Exception {
        VLOOKUP v = new VLOOKUP();
        fail("VLOOKUP not implemented");
    }

    public void testWEEKDAY() throws Exception {
        WEEKDAY w = new WEEKDAY();
        fail("WEEKDAY not implemented");
    }

    public void testWEIBULL() throws Exception {
        WEIBULL w = new WEIBULL();
        fail("WEIBULL not implemented");
    }

    public void testYEAR() throws Exception {
        YEAR y = new YEAR();
        fail("YEAR not implemented");
    }

    public void testZTEST() throws Exception {
        ZTEST z = new ZTEST();
        fail("ZTEST not implemented");
    }
}
