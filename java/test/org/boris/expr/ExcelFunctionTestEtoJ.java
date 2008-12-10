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

import org.boris.expr.function.excel.ERRORTYPE;
import org.boris.expr.function.excel.EVEN;
import org.boris.expr.function.excel.EXACT;
import org.boris.expr.function.excel.EXP;
import org.boris.expr.function.excel.EXPONDIST;
import org.boris.expr.function.excel.FACT;
import org.boris.expr.function.excel.FALSE;
import org.boris.expr.function.excel.FDIST;
import org.boris.expr.function.excel.FIND;
import org.boris.expr.function.excel.FINV;
import org.boris.expr.function.excel.FISHER;
import org.boris.expr.function.excel.FISHERNV;
import org.boris.expr.function.excel.FORECAST;
import org.boris.expr.function.excel.FREQUENCY;
import org.boris.expr.function.excel.FTEST;
import org.boris.expr.function.excel.FV;
import org.boris.expr.function.excel.GAMMADIST;
import org.boris.expr.function.excel.GAMMAINV;
import org.boris.expr.function.excel.GAMMALN;
import org.boris.expr.function.excel.GEOMEAN;
import org.boris.expr.function.excel.GETPIVOTDATA;
import org.boris.expr.function.excel.GROWTH;
import org.boris.expr.function.excel.HARMEAN;
import org.boris.expr.function.excel.HLOOKUP;
import org.boris.expr.function.excel.HOUR;
import org.boris.expr.function.excel.HYPERLINK;
import org.boris.expr.function.excel.HYPGEOMDIST;
import org.boris.expr.function.excel.IF;
import org.boris.expr.function.excel.INDEX;
import org.boris.expr.function.excel.INDIRECT;
import org.boris.expr.function.excel.INFO;
import org.boris.expr.function.excel.INT;
import org.boris.expr.function.excel.INTERCEPT;
import org.boris.expr.function.excel.IPMT;
import org.boris.expr.function.excel.IRR;
import org.boris.expr.function.excel.ISBLANK;
import org.boris.expr.function.excel.ISERR;
import org.boris.expr.function.excel.ISLOGICAL;
import org.boris.expr.function.excel.ISNA;
import org.boris.expr.function.excel.ISNONTEXT;
import org.boris.expr.function.excel.ISNUMBER;
import org.boris.expr.function.excel.ISPMT;
import org.boris.expr.function.excel.ISREF;
import org.boris.expr.function.excel.ISTEXT;

public class ExcelFunctionTestEtoJ extends TH
{
    public void testERRORTYPE() throws Exception {
        ERRORTYPE e = new ERRORTYPE();
        assertEquals(eval(e, ExprError.NULL), 1);
        assertEquals(eval(e, ExprError.DIV0), 2);
        assertEquals(eval(e, ExprError.VALUE), 3);
        assertEquals(eval(e, ExprError.VALUE), 3);
        assertEquals(eval(e, "asdf"), ExprError.NA);
    }

    public void testEVEN() throws Exception {
        EVEN e = new EVEN();
        assertEquals(eval(e, -10.1), -12.);
        assertEquals(eval(e, -9.9), -10.);
        assertEquals(eval(e, -8.01), -10.);
        assertEquals(eval(e, -7.5), -8.);
        assertEquals(eval(e, -6), -6.);
        assertEquals(eval(e, -5), -6.);
        assertEquals(eval(e, -4), -4.);
        assertEquals(eval(e, -3), -4.);
        assertEquals(eval(e, -2), -2.);
        assertEquals(eval(e, -1), -2.);
        assertEquals(eval(e, -0.), -0.);
        assertEquals(eval(e, 0), 0.);
        assertEquals(eval(e, 5.5), 6.);
        assertEquals(eval(e, 4), 4.);
        assertEquals(eval(e, 2.01), 4.);
        assertEquals(eval(e, 5), 6.);
        assertEquals(eval(e, 6), 6.);
    }

    public void testEXACT() throws Exception {
        EXACT e = new EXACT();
        assertEquals(eval(e, "word", "Word"), false);
        assertEquals(eval(e, "word", "word"), true);
        assertEquals(eval(e, "1.2", 1.2), true);
        assertEquals(eval(e, ExprMissing.MISSING, ExprMissing.MISSING), true);
    }

    public void testEXP() throws Exception {
        EXP e = new EXP();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 10;
            assertEquals(eval(e, d), Math.exp(d));
        }
    }

    public void testEXPONDIST() throws Exception {
        EXPONDIST e = new EXPONDIST();
        fail("EXPONDIST not implemented");
    }

    public void testFACT() throws Exception {
        FACT f = new FACT();
        assertEquals(eval(f, 5), 120.);
        assertEquals(eval(f, 1.9), 1.);
        assertEquals(eval(f, 0), 1.);
        assertEquals(eval(f, -1), ExprError.NUM);
        assertEquals(eval(f, 1), 1.);
    }

    public void testFALSE() throws Exception {
        FALSE f = new FALSE();
        assertException("false(1)");
        assertEquals(eval(f), false);
    }

    public void testFDIST() throws Exception {
        FDIST f = new FDIST();
        fail("FDIST not implemented");
    }

    public void testFIND() throws Exception {
        FIND f = new FIND();
        fail("FIND not implemented");
    }

    public void testFINV() throws Exception {
        FINV f = new FINV();
        fail("FINV not implemented");
    }

    public void testFISHER() throws Exception {
        FISHER f = new FISHER();
        fail("FISHER not implemented");
    }

    public void testFISHERNV() throws Exception {
        FISHERNV f = new FISHERNV();
        fail("FISHERNV not implemented");
    }

    public void testFIXED() throws Exception {
        assertResult("fixed( 1234.567, 1)", "1,234.6");
        assertResult(" fixed(1234.567,-1)", "1,230");
        assertResult("FIXED(-1234.567, -1, TRUE)", "-1230");
        assertResult("FIXED(44.332)", "44.33");
    }

    public void testFLOOR() throws Exception {
        assertResult("FLOOR(2.5, 1)", 2.);
        assertResult("FLOOR(-2.5, -2)", -2.);
        assertResult("FLOOR(-2.5, 2)", ExprError.NUM);
        assertResult("FLOOR(1.5, 0.1)", 1.5);
        assertResult("FLOOR(0.234, 0.01)", 0.23);
    }

    public void testFORECAST() throws Exception {
        FORECAST f = new FORECAST();
        fail("FORECAST not implemented");
    }

    public void testFREQUENCY() throws Exception {
        FREQUENCY f = new FREQUENCY();
        fail("FREQUENCY not implemented");
    }

    public void testFTEST() throws Exception {
        FTEST f = new FTEST();
        fail("FTEST not implemented");
    }

    public void testFV() throws Exception {
        FV f = new FV();
        fail("FV not implemented");
    }

    public void testGAMMADIST() throws Exception {
        GAMMADIST g = new GAMMADIST();
        fail("GAMMADIST not implemented");
    }

    public void testGAMMAINV() throws Exception {
        GAMMAINV g = new GAMMAINV();
        fail("GAMMAINV not implemented");
    }

    public void testGAMMALN() throws Exception {
        GAMMALN g = new GAMMALN();
        fail("GAMMALN not implemented");
    }

    public void testGEOMEAN() throws Exception {
        GEOMEAN g = new GEOMEAN();
        fail("GEOMEAN not implemented");
    }

    public void testGETPIVOTDATA() throws Exception {
        GETPIVOTDATA g = new GETPIVOTDATA();
        fail("GETPIVOTDATA not implemented");
    }

    public void testGROWTH() throws Exception {
        GROWTH g = new GROWTH();
        fail("GROWTH not implemented");
    }

    public void testHARMEAN() throws Exception {
        HARMEAN h = new HARMEAN();
        fail("HARMEAN not implemented");
    }

    public void testHLOOKUP() throws Exception {
        HLOOKUP h = new HLOOKUP();
        fail("HLOOKUP not implemented");
    }

    public void testHOUR() throws Exception {
        HOUR h = new HOUR();
        fail("HOUR not implemented");
    }

    public void testHYPERLINK() throws Exception {
        HYPERLINK h = new HYPERLINK();
        fail("HYPERLINK not implemented");
    }

    public void testHYPGEOMDIST() throws Exception {
        HYPGEOMDIST h = new HYPGEOMDIST();
        fail("HYPGEOMDIST not implemented");
    }

    public void testIF() throws Exception {
        IF i = new IF();
        assertResult("if(10<100,4,5)", 4);
        assertEquals(eval(i, parse("10.1>=11"), "right", "wrong"), "wrong");
    }

    public void testINDEX() throws Exception {
        INDEX i = new INDEX();
        fail("INDEX not implemented");
    }

    public void testINDIRECT() throws Exception {
        INDIRECT i = new INDIRECT();
        fail("INDIRECT not implemented");
    }

    public void testINFO() throws Exception {
        INFO i = new INFO();
        fail("INFO not implemented");
    }

    public void testINT() throws Exception {
        INT i = new INT();
        assertEquals(eval(i, 1.9), 1);
        assertEquals(eval(i, -1.9), -2);
        assertEquals(eval(i, -2.0), -2);
        assertEquals(eval(i, 8.9), 8);
        assertEquals(eval(i, -8.9), -9);
        assertResult("19.5-int(19.5)", 0.5);
    }

    public void testINTERCEPT() throws Exception {
        INTERCEPT i = new INTERCEPT();
        fail("INTERCEPT not implemented");
    }

    public void testIPMT() throws Exception {
        IPMT i = new IPMT();
        fail("IPMT not implemented");
    }

    public void testIRR() throws Exception {
        IRR i = new IRR();
        fail("IRR not implemented");
    }

    public void testISBLANK() throws Exception {
        ISBLANK i = new ISBLANK();
        fail("ISBLANK not implemented");
    }

    public void testISERR() throws Exception {
        ISERR i = new ISERR();
        fail("ISERR not implemented");
    }

    public void testISLOGICAL() throws Exception {
        ISLOGICAL i = new ISLOGICAL();
        fail("ISLOGICAL not implemented");
    }

    public void testISNA() throws Exception {
        ISNA i = new ISNA();
        fail("ISNA not implemented");
    }

    public void testISNONTEXT() throws Exception {
        ISNONTEXT i = new ISNONTEXT();
        fail("ISNONTEXT not implemented");
    }

    public void testISNUMBER() throws Exception {
        ISNUMBER i = new ISNUMBER();
        fail("ISNUMBER not implemented");
    }

    public void testISPMT() throws Exception {
        ISPMT i = new ISPMT();
        fail("ISPMT not implemented");
    }

    public void testISREF() throws Exception {
        ISREF i = new ISREF();
        fail("ISREF not implemented");
    }

    public void testISTEXT() throws Exception {
        ISTEXT i = new ISTEXT();
        fail("ISTEXT not implemented");
    }
}
