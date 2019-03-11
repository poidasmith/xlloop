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

import org.boris.expr.function.excel.CELL;
import org.boris.expr.function.excel.ERRORTYPE;
import org.boris.expr.function.excel.INFO;
import org.boris.expr.function.excel.ISBLANK;
import org.boris.expr.function.excel.ISERR;
import org.boris.expr.function.excel.ISLOGICAL;
import org.boris.expr.function.excel.ISNA;
import org.boris.expr.function.excel.ISNONTEXT;
import org.boris.expr.function.excel.ISNUMBER;
import org.boris.expr.function.excel.ISTEXT;
import org.boris.expr.function.excel.N;
import org.boris.expr.function.excel.NA;
import org.boris.expr.function.excel.TYPE;

public class ExcelInformationFunctionsTest extends TH
{
    public void testCELL() throws Exception {
        CELL c = new CELL();
        fail("CELL not implemented");
    }

    public void testERRORTYPE() throws Exception {
        ERRORTYPE e = new ERRORTYPE();
        assertEquals(eval(e, ExprError.NULL), 1);
        assertEquals(eval(e, ExprError.DIV0), 2);
        assertEquals(eval(e, ExprError.VALUE), 3);
        assertEquals(eval(e, ExprError.VALUE), 3);
        assertEquals(eval(e, "asdf"), ExprError.NA);
    }

    public void testINFO() throws Exception {
        INFO i = new INFO();
        fail("INFO not implemented");
    }

    public void testISBLANK() throws Exception {
        ISBLANK i = new ISBLANK();
        assertEquals(eval(i, ""), true);
        assertEquals(eval(i, ExprMissing.MISSING), true);
        assertEquals(eval(i, ExprError.DIV0), false);
        assertEquals(eval(i, "#DIV/0!"), false);
    }

    public void testISERR() throws Exception {
        ISERR i = new ISERR();
        assertEquals(eval(i, true), false);
        assertEquals(eval(i, false), false);
        assertEquals(eval(i, ExprError.DIV0), true);
        assertEquals(eval(i, "#DIV/0!"), false);
    }

    public void testISLOGICAL() throws Exception {
        ISLOGICAL i = new ISLOGICAL();
        assertEquals(eval(i, true), true);
        assertEquals(eval(i, false), true);
        assertEquals(eval(i, 1), false);
        assertEquals(eval(i, 1.0), false);
    }

    public void testISNA() throws Exception {
        ISNA i = new ISNA();
        assertEquals(eval(i, true), false);
        assertEquals(eval(i, ""), false);
        assertEquals(eval(i, 1), false);
        assertEquals(eval(i, 1.0), false);
        assertEquals(eval(i, toArray(1)), false);
        assertEquals(eval(i, ExprError.DIV0), false);
        assertEquals(eval(i, ExprError.NA), true);
    }

    public void testISNONTEXT() throws Exception {
        ISNONTEXT i = new ISNONTEXT();
        assertEquals(eval(i, true), true);
        assertEquals(eval(i, ""), true);
        assertEquals(eval(i, "asdf"), false);
    }

    public void testISNUMBER() throws Exception {
        ISNUMBER i = new ISNUMBER();
        assertEquals(eval(i, ""), false);
        assertEquals(eval(i, 1), true);
        assertEquals(eval(i, 1.), true);
        assertEquals(eval(i, false), false);
    }

    public void testISREF() throws Exception {
        assertResult("ISREF(A1)", true);
        assertResult("ISREF(1)", false);
    }

    public void testISTEXT() throws Exception {
        ISTEXT i = new ISTEXT();
        assertEquals(eval(i, ""), true);
        assertEquals(eval(i, 1), false);
        assertEquals(eval(i, 1.), false);
        assertEquals(eval(i, false), false);
        assertEquals(eval(i, toArray("asdF")), false);
    }

    public void testN() throws Exception {
        N n = new N();
        assertEquals(eval(n, 1.0), 1.);
        assertEquals(eval(n, "asf"), 0.);
        assertEquals(eval(n, true), 1.0);
        assertEquals(eval(n, false), 0.);
        assertException("n(234,2)");
    }

    public void testNA() throws Exception {
        NA n = new NA();
        assertEquals(eval(n), ExprError.NA);
        assertException("na(1)");
        assertResult("na()", ExprError.NA);
    }

    public void testTYPE() throws Exception {
        TYPE t = new TYPE();
        assertEquals(eval(t, "asdf"), 2.);
        assertEquals(eval(t, 2), 1.);
        assertEquals(eval(t, ExprError.VALUE), 16.);
        assertEquals(eval(t, toArray(1, 1)), 64.);
    }
}
