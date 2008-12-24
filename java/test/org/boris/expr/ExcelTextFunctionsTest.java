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

import org.boris.expr.function.excel.BAHTTEXT;
import org.boris.expr.function.excel.CHAR;
import org.boris.expr.function.excel.CODE;
import org.boris.expr.function.excel.CONCATENATE;
import org.boris.expr.function.excel.DOLLAR;
import org.boris.expr.function.excel.EXACT;
import org.boris.expr.function.excel.FIND;
import org.boris.expr.function.excel.LEFT;
import org.boris.expr.function.excel.LEN;
import org.boris.expr.function.excel.LOWER;
import org.boris.expr.function.excel.MID;
import org.boris.expr.function.excel.PROPER;
import org.boris.expr.function.excel.REPLACE;
import org.boris.expr.function.excel.REPT;
import org.boris.expr.function.excel.RIGHT;
import org.boris.expr.function.excel.SEARCH;
import org.boris.expr.function.excel.TEXT;
import org.boris.expr.function.excel.TRIM;
import org.boris.expr.function.excel.UPPER;

public class ExcelTextFunctionsTest extends TH
{
    public void testBAHTTEXT() throws Exception {
        BAHTTEXT b = new BAHTTEXT();
        fail("BAHTTEXT not implemented");
    }

    public void testCHAR() throws Exception {
        CHAR c = new CHAR();
        assertEquals(eval(c, 65), "A");
        assertEquals(eval(c, 33), "!");
    }

    public void testCLEAN() throws Exception {
        assertResult("CLEAN(CHAR(7)&\"text\"&CHAR(7))", "text");
    }

    public void testCODE() throws Exception {
        CODE c = new CODE();
        assertEquals(eval(c, true), 84);
        assertEquals(eval(c, false), 70);
        assertEquals(eval(c, 1), 49);
        assertEquals(eval(c, "asd"), 97);
        assertEquals(eval(c, "%$"), 37);
        assertEquals(eval(c, 56.3), 53);
    }

    public void testCONCATENATE() throws Exception {
        CONCATENATE c = new CONCATENATE();
        assertEquals(eval(c, "asdf"), "asdf");
        assertEquals(eval(c, "asdf", "qwer"), "asdfqwer");
        assertEquals(eval(c, 1, 2, 3.), "123.0");
    }

    public void testDOLLAR() throws Exception {
        DOLLAR d = new DOLLAR();
        fail("DOLLAR not implemented");
    }

    public void testEXACT() throws Exception {
        EXACT e = new EXACT();
        assertEquals(eval(e, "word", "Word"), false);
        assertEquals(eval(e, "word", "word"), true);
        assertEquals(eval(e, "1.2", 1.2), true);
        assertEquals(eval(e, ExprMissing.MISSING, ExprMissing.MISSING), true);
    }

    public void testFIND() throws Exception {
        FIND f = new FIND();
        assertEquals(eval(f, "M", "Miriam McGovern"), 1);
        assertEquals(eval(f, "m", "Miriam McGovern"), 6);
        assertEquals(eval(f, "M", "Miriam McGovern", 3), 8);
    }

    public void testFIXED() throws Exception {
        assertResult("fixed( 1234.567, 1)", "1,234.6");
        assertResult(" fixed(1234.567,-1)", "1,230");
        assertResult("FIXED(-1234.567, -1, TRUE)", "-1230");
        assertResult("FIXED(44.332)", "44.33");
    }

    public void testLEFT() throws Exception {
        LEFT l = new LEFT();
        assertEquals(eval(l, "Sale Price", 4), "Sale");
        assertEquals(eval(l, "Sweden", 1), "S");
    }

    public void testLEN() throws Exception {
        LEN l = new LEN();
        assertEquals(eval(l, "Phoenix, AZ"), 11.);
        assertEquals(eval(l, ""), 0.);
        assertEquals(eval(l, "   One  "), 8.);
    }

    public void testLOWER() throws Exception {
        LOWER l = new LOWER();
        assertEquals(eval(l, "ASDF"), "asdf");
        assertEquals(eval(l, true), "true");
        assertEquals(eval(l, 1), "1");
        assertException("lower(1,1)");
        assertResult("lower({1,2})", ExprError.VALUE);
    }

    public void testMID() throws Exception {
        MID m = new MID();
        assertEquals(eval(m, "Fluid Flow", 1, 5), "Fluid");
        assertEquals(eval(m, "Fluid Flow", 7, 20), "Flow");
        assertEquals(eval(m, "Fluid Flow", 20, 5), "");
        assertEquals(eval(m, "Fluid Flow", -1, 5), ExprError.VALUE);
        assertEquals(eval(m, "Fluid Flow", 0, 5), ExprError.VALUE);
        assertEquals(eval(m, "Fluid Flow", 4, -2), ExprError.VALUE);
    }

    public void testPROPER() throws Exception {
        PROPER p = new PROPER();
        assertEquals(eval(p, "this is a TITLE"), "This Is A Title");
        assertEquals(eval(p, "2-cent\'s worth"), "2-Cent\'S Worth");
        assertEquals(eval(p, "76BudGet"), "76Budget");
    }

    public void testREPLACE() throws Exception {
        REPLACE r = new REPLACE();
        assertEquals(eval(r, "abcdefghijk", 6, 5, "*"), "abcde*k");
        assertEquals(eval(r, "2009", 3, 2, "10"), "2010");
        assertEquals(eval(r, "123456", 1, 3, "@"), "@456");
    }

    public void testREPT() throws Exception {
        REPT r = new REPT();
        assertResult("REPT({1},3.2)", "111");
        assertEquals(eval(r, "*-", 3), "*-*-*-");
        assertEquals(eval(r, "-", 10), "----------");
        assertException("rept(1,1,1)");
    }

    public void testRIGHT() throws Exception {
        RIGHT r = new RIGHT();
        assertEquals(eval(r, "Sale Price", 5), "Price");
        assertEquals(eval(r, "Stock Number"), "r");
    }

    public void testSEARCH() throws Exception {
        SEARCH s = new SEARCH();
        fail("SEARCH not implemented");
    }

    public void testSUBSTITUE() throws Exception {
        BasicEvaluationCallback c = new BasicEvaluationCallback();
        c.set("A2", "Sales Data");
        c.set("A3", "Quarter 1, 2008");
        c.set("A4", "Quarter 1, 2011");
        assertResult(c, "SUBSTITUTE(A2, \"Sales\", \"Cost\")", "Cost Data");
        assertResult(c, "SUBSTITUTE(A3, \"1\", \"2\", 1)", "Quarter 2, 2008");
        assertResult(c, "SUBSTITUTE(A4, \"1\", \"2\", 3)", "Quarter 1, 2012");
    }

    public void testT() throws Exception {
        assertResult("T(1)", "");
        assertResult("T(true)", "");
        assertResult("T(\"asdf\")", "asdf");
        assertException("T(123,24)");
    }

    public void testTEXT() throws Exception {
        TEXT t = new TEXT();
        fail("TEXT not implemented");
    }

    public void testTRIM() throws Exception {
        TRIM t = new TRIM();
        assertEquals(eval(t, " First Quarter Earnings "),
                "First Quarter Earnings");
        assertEquals(eval(t, ""), "");
        assertEquals(eval(t, "   "), "");
    }

    public void testUPPER() throws Exception {
        UPPER u = new UPPER();
        assertEquals(eval(u, "asdf"), "ASDF");
        assertEquals(eval(u, true), "TRUE");
        assertEquals(eval(u, 1), "1");
        assertException("upper(1,1)");
        assertResult("upper({1,2})", ExprError.VALUE);
    }

    public void testVALUE() throws Exception {
        assertResult("VALUE(\"16:48:00\")-VALUE(\"12:00:00\")", 0.2);
        assertResult("VALUE(\"$1,000\")", 1000.);
    }
}
