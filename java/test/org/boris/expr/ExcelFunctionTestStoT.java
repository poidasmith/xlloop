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

import org.boris.expr.function.AbstractFunction;
import org.boris.expr.function.excel.SEARCH;
import org.boris.expr.function.excel.SECOND;
import org.boris.expr.function.excel.SIGN;
import org.boris.expr.function.excel.SIN;
import org.boris.expr.function.excel.SINH;
import org.boris.expr.function.excel.SKEW;
import org.boris.expr.function.excel.SLN;
import org.boris.expr.function.excel.SLOPE;
import org.boris.expr.function.excel.SMALL;
import org.boris.expr.function.excel.SQRT;
import org.boris.expr.function.excel.STANDARDIZE;
import org.boris.expr.function.excel.STDEVA;
import org.boris.expr.function.excel.STDEVPA;
import org.boris.expr.function.excel.STEYX;
import org.boris.expr.function.excel.SUBSTITUE;
import org.boris.expr.function.excel.SUBTOTAL;
import org.boris.expr.function.excel.SUMIF;
import org.boris.expr.function.excel.SUMPRODUCT;
import org.boris.expr.function.excel.SUMSQ;
import org.boris.expr.function.excel.SUMX2MY2;
import org.boris.expr.function.excel.SUMX2PY2;
import org.boris.expr.function.excel.SUMXMY2;
import org.boris.expr.function.excel.SYD;
import org.boris.expr.function.excel.TAN;
import org.boris.expr.function.excel.TANH;
import org.boris.expr.function.excel.TDIST;
import org.boris.expr.function.excel.TEXT;
import org.boris.expr.function.excel.TIME;
import org.boris.expr.function.excel.TIMEVALUE;
import org.boris.expr.function.excel.TINV;
import org.boris.expr.function.excel.TODAY;
import org.boris.expr.function.excel.TRANSPOSE;
import org.boris.expr.function.excel.TREND;
import org.boris.expr.function.excel.TRIMMEAN;
import org.boris.expr.function.excel.TRUNC;
import org.boris.expr.function.excel.TTEST;
import org.boris.expr.function.excel.TYPE;
import org.boris.expr.util.ExcelDate;

public class ExcelFunctionTestStoT extends TH
{
    public void testSEARCH() throws Exception {
        SEARCH s = new SEARCH();
        fail("SEARCH not implemented");
    }

    public void testSECOND() throws Exception {
        SECOND s = new SECOND();
        fail("SECOND not implemented");
    }

    public void testSIGN() throws Exception {
        SIGN s = new SIGN();
        assertEquals(eval(s, 10), 1.);
        assertEquals(eval(s, 4 - 4), 0.);
        assertEquals(eval(s, -0.00001), -1.);
    }

    public void testSIN() throws Exception {
        SIN s = new SIN();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000;
            assertEquals("SIN not working", eval(s, d), Math.sin(d));
        }
    }

    public void testSINH() throws Exception {
        SINH s = new SINH();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000;
            assertEquals("SINH not working", eval(s, d), Math.sinh(d));
        }
    }

    public void testSKEW() throws Exception {
        SKEW s = new SKEW();
        fail("SKEW not implemented");
    }

    public void testSLN() throws Exception {
        SLN s = new SLN();
        fail("SLN not implemented");
    }

    public void testSLOPE() throws Exception {
        SLOPE s = new SLOPE();
        fail("SLOPE not implemented");
    }

    public void testSMALL() throws Exception {
        SMALL s = new SMALL();
        fail("SMALL not implemented");
    }

    public void testSQRT() throws Exception {
        SQRT s = new SQRT();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000;
            assertEquals("SQRT not working", eval(s, d), Math.sqrt(d));
        }
    }

    public void testSTANDARDIZE() throws Exception {
        STANDARDIZE s = new STANDARDIZE();
        fail("STANDARDIZE not implemented");
    }

    public void testSTDEV() throws Exception {
        assertResult(
                "stdev({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                27.4639157198405);
    }

    public void testSTDEVA() throws Exception {
        STDEVA s = new STDEVA();
        fail("STDEVA not implemented");
    }

    public void testSTDEVP() throws Exception {
        assertResult(
                "stdevp({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                26.0545581424796);
    }

    public void testSTDEVPA() throws Exception {
        STDEVPA s = new STDEVPA();
        fail("STDEVPA not implemented");
    }

    public void testSTEYX() throws Exception {
        STEYX s = new STEYX();
        fail("STEYX not implemented");
    }

    public void testSUBSTITUE() throws Exception {
        SUBSTITUE s = new SUBSTITUE();
        fail("SUBSTITUE not implemented");
    }

    public void testSUBTOTAL() throws Exception {
        SUBTOTAL s = new SUBTOTAL();
        fail("SUBTOTAL not implemented");
    }

    public void testSUM() throws Exception {
        assertResult("sum(34,45)", 79.);
    }

    public void testSUMIF() throws Exception {
        AbstractFunction s = new SUMIF();
        assertEquals(eval(s, toArray(1, 2, 3), ">2"), 3.);
        assertEquals(eval(s, toArray(1.658, 1.656, 1.657), "<1.657"), 1.656);
        assertEquals(eval(s, toArray(1, 2, 3), "<=2"), 3.);
        assertEquals(eval(s, toArray(1, 2, 3), ">=2"), 5.);
        assertEquals(eval(s, toArray("apples", 2, 3), "apples", toArray(4.5)),
                4.5);
        assertEquals(eval(s, toArray(1, 2.5, 3), "=2.5", toArray(3, 2, 1)), 2.);
        assertEquals(eval(s, toArray(1, 2.5, 3), 1, toArray(3, 2, 1)), 3.);
    }

    public void testSUMPRODUCT() throws Exception {
        SUMPRODUCT s = new SUMPRODUCT();
        assertEquals(eval(s, toArray(1, 2, 3), toArray(4, 5, 6)), 32.);
        assertEquals(eval(s, toArray(3., 8, 1, 4., 6, 9), toArray(2, 6, 5, 7,
                7, 3)), 156.);
    }

    public void testSUMSQ() throws Exception {
        SUMSQ s = new SUMSQ();
        assertEquals(eval(s, 3, 4), 25.);
    }

    public void testSUMX2MY2() throws Exception {
        SUMX2MY2 s = new SUMX2MY2();
        fail("SUMX2MY2 not implemented");
    }

    public void testSUMX2PY2() throws Exception {
        SUMX2PY2 s = new SUMX2PY2();
        fail("SUMX2PY2 not implemented");
    }

    public void testSUMXMY2() throws Exception {
        SUMXMY2 s = new SUMXMY2();
        fail("SUMXMY2 not implemented");
    }

    public void testSYD() throws Exception {
        SYD s = new SYD();
        assertEquals(eval(s, 30000, 7500, 10, 1), 4090.90909090909);
        assertEquals(eval(s, 30000, 7500, 10, 10), 409.090909090909);
    }

    public void testT() throws Exception {
        assertResult("T(1)", "");
        assertResult("T(true)", "");
        assertResult("T(\"asdf\")", "asdf");
        assertException("T(123,24)");
    }

    public void testTAN() throws Exception {
        TAN t = new TAN();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000;
            assertEquals("TAN not working", eval(t, d), Math.tan(d));
        }
    }

    public void testTANH() throws Exception {
        TANH t = new TANH();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000;
            assertEquals("TANH not working", eval(t, d), Math.tanh(d));
        }
    }

    public void testTDIST() throws Exception {
        TDIST t = new TDIST();
        fail("TDIST not implemented");
    }

    public void testTEXT() throws Exception {
        TEXT t = new TEXT();
        fail("TEXT not implemented");
    }

    public void testTIME() throws Exception {
        TIME t = new TIME();
        fail("TIME not implemented");
    }

    public void testTIMEVALUE() throws Exception {
        TIMEVALUE t = new TIMEVALUE();
        fail("TIMEVALUE not implemented");
    }

    public void testTINV() throws Exception {
        TINV t = new TINV();
        fail("TINV not implemented");
    }

    public void testTODAY() throws Exception {
        TODAY t = new TODAY();
        for (int i = 0; i < 100; i++) {
            assertEquals("TODAY not working", eval(t), ExcelDate
                    .toExcelDate(new Date().getTime()));
        }
    }

    public void testTRANSPOSE() throws Exception {
        TRANSPOSE t = new TRANSPOSE();
        fail("TRANSPOSE not implemented");
    }

    public void testTREND() throws Exception {
        TREND t = new TREND();
        fail("TREND not implemented");
    }

    public void testTRIMMEAN() throws Exception {
        TRIMMEAN t = new TRIMMEAN();
        fail("TRIMMEAN not implemented");
    }

    public void testTRUE() throws Exception {
        assertResult("true()", ExprBoolean.TRUE);
        assertException("true(1)");
        assertException("true(234,34,34)");
    }

    public void testTRUNC() throws Exception {
        TRUNC t = new TRUNC();
        fail("TRUNC not implemented");
    }

    public void testTTEST() throws Exception {
        TTEST t = new TTEST();
        fail("TTEST not implemented");
    }

    public void testTYPE() throws Exception {
        TYPE t = new TYPE();
        fail("TYPE not implemented");
    }
}
