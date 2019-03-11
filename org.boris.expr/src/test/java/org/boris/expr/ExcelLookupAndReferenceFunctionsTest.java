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

import org.boris.expr.function.excel.ADDRESS;
import org.boris.expr.function.excel.CHOOSE;
import org.boris.expr.function.excel.GETPIVOTDATA;
import org.boris.expr.function.excel.HLOOKUP;
import org.boris.expr.function.excel.HYPERLINK;
import org.boris.expr.function.excel.INDIRECT;
import org.boris.expr.function.excel.MATCH;
import org.boris.expr.function.excel.RTD;
import org.boris.expr.function.excel.VLOOKUP;

public class ExcelLookupAndReferenceFunctionsTest extends TH
{
    public void testADDRESS() throws Exception {
        ADDRESS a = new ADDRESS();
        assertEquals(eval(a, 1, 1, 2, 0, "Sheet "), "\'Sheet \'!R1C[1]");
        assertEquals(eval(a, 2, 3), "$C$2");
        assertEquals(eval(a, 2, 3, 2), "C$2");
        assertEquals(eval(a, 2, 3, 2, false), "R2C[3]");
        assertEquals(eval(a, 2, 3, 1, false, "EXCEL SHEET"),
                "\'EXCEL SHEET\'!R2C3");
    }

    public void testAREAS() throws Exception {
        assertResult("AREAS(B4:C7)", 1.);
        assertResult("AREAS(B2:D4)", 1.);
        assertResult("AREAS((B2:D4,E5,F6:I9))", 3.);
        assertResult("AREAS(B2:D4 B2)", 1.);
    }

    public void testCHOOSE() throws Exception {
        CHOOSE c = new CHOOSE();
        assertEquals(eval(c, 1, 3, 4), 3);
        assertEquals(eval(c, 2, 3, "hello"), "hello");
        assertEquals(eval(c, true, 35), 35);
        assertException(c, 1.2);
        assertException(c, 1);
        assertException(c, "asdf");
        assertException(c, 0, 23, 34);
        assertException(c);
    }

    public void testCOLUMN() throws Exception {
        assertResult("column(A1)", 1);
        assertResult("column(fas)", ExprError.NAME);
    }

    public void testCOLUMNS() throws Exception {
        assertResult("columns(a3:d4)", 4);
        assertResult("columns(1)", 1);
        assertResult("columns(X3)", 1);
        assertResult("columns(a-1)", ExprError.NAME);
    }

    public void testGETPIVOTDATA() throws Exception {
        GETPIVOTDATA g = new GETPIVOTDATA();
        fail("GETPIVOTDATA not implemented");
    }

    public void testHLOOKUP() throws Exception {
        HLOOKUP h = new HLOOKUP();
        assertResult(
                "HLOOKUP(3,{1,2,3;\"a\",\"b\",\"c\";\"d\",\"e\",\"f\"},2,TRUE)",
                "c");
        fail("HLOOKUP not implemented");
    }

    public void testHYPERLINK() throws Exception {
        HYPERLINK h = new HYPERLINK();
        fail("HYPERLINK not implemented");
    }

    public void testINDEX() throws Exception {
        BasicEvaluationCallback c = new BasicEvaluationCallback();
        c.set(loadArray("index1.txt"));
        assertResult(c, "INDEX(A2:C6,2,3)", 38);
        assertResult(c, "INDEX((A1:C6,A8:C11),2,2,2)", 3.55);
        assertResult(c, "SUM(INDEX(A1:C11,0,3,1))", 216);
        assertResult(c, "SUM(B2:INDEX(A2:C6,5,2))", 2.42);
    }

    public void testINDIRECT() throws Exception {
        INDIRECT i = new INDIRECT();
        fail("INDIRECT not implemented");
    }

    public void testLOOKUP() throws Exception {
        assertResult("LOOKUP(\"C\",{\"a\",\"b\",\"c\",\"d\";1,2,3,4})", 3);
        assertResult("LOOKUP(\"bump\",{\"a\",1;\"b\",2;\"c\",3})", 2);
        BasicEvaluationCallback c = new BasicEvaluationCallback();
        c.set(loadArray("lookup1.txt"));
        assertResult(c, "LOOKUP(4.91,A2:A6,B2:B6)", "orange");
        assertResult(c, "LOOKUP(5,A2:A6,B2:B6)", "orange");
        assertResult(c, "LOOKUP(7.66,A2:A6,B2:B6)", "blue");
        assertResult(c, "LOOKUP(0,A2:A6,B2:B6)", ExprError.NA);
    }

    public void testMATCH() throws Exception {
        MATCH m = new MATCH();
        fail("MATCH not implemented");
    }

    public void testOFFSET() throws Exception {
        assertResult("offset(c3,2,3,1,1)", var("F5"), true);
        assertResult("offset(c3:e5,-1,0,3,3)", var("C2:E4"), true);
        assertResult("offset(c3:e5,0,-3,3,3)", ExprError.REF, true);
    }

    public void testROW() throws Exception {
        assertResult("row(A1)", 1);
        assertResult("row(fas)", ExprError.NAME);
    }

    public void testROWS() throws Exception {
        assertResult("rows(a3:d4)", 2);
        assertResult("rows(1)", 1);
        assertResult("rows(X3)", 1);
        assertResult("rows(a-1)", ExprError.NAME);
    }

    public void testRTD() throws Exception {
        RTD r = new RTD();
        fail("RTD not implemented");
    }

    public void testTRANSPOSE() throws Exception {
        assertResult("transpose(1)", 1);
        assertResult("transpose(\"asdf\")", "asdf");
        assertResult("transpose({1,2;3,4})", parse("{1, 3; 2, 4}"));
    }

    public void testVLOOKUP() throws Exception {
        VLOOKUP v = new VLOOKUP();
        fail("VLOOKUP not implemented");
    }
}
