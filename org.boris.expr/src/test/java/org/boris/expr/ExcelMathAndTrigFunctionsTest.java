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

import org.boris.expr.function.AbstractFunction;
import org.boris.expr.function.excel.ABS;
import org.boris.expr.function.excel.ACOS;
import org.boris.expr.function.excel.ACOSH;
import org.boris.expr.function.excel.ASIN;
import org.boris.expr.function.excel.ASINH;
import org.boris.expr.function.excel.ATAN;
import org.boris.expr.function.excel.ATAN2;
import org.boris.expr.function.excel.ATANH;
import org.boris.expr.function.excel.CEILING;
import org.boris.expr.function.excel.COMBIN;
import org.boris.expr.function.excel.COS;
import org.boris.expr.function.excel.COSH;
import org.boris.expr.function.excel.DEGREES;
import org.boris.expr.function.excel.EVEN;
import org.boris.expr.function.excel.EXP;
import org.boris.expr.function.excel.FACT;
import org.boris.expr.function.excel.INT;
import org.boris.expr.function.excel.LN;
import org.boris.expr.function.excel.LOG;
import org.boris.expr.function.excel.LOG10;
import org.boris.expr.function.excel.MDETERM;
import org.boris.expr.function.excel.MINVERSE;
import org.boris.expr.function.excel.MMULT;
import org.boris.expr.function.excel.MOD;
import org.boris.expr.function.excel.ODD;
import org.boris.expr.function.excel.PI;
import org.boris.expr.function.excel.POWER;
import org.boris.expr.function.excel.RADIANS;
import org.boris.expr.function.excel.RAND;
import org.boris.expr.function.excel.ROMAN;
import org.boris.expr.function.excel.ROUNDUP;
import org.boris.expr.function.excel.SIGN;
import org.boris.expr.function.excel.SIN;
import org.boris.expr.function.excel.SINH;
import org.boris.expr.function.excel.SQRT;
import org.boris.expr.function.excel.SUBTOTAL;
import org.boris.expr.function.excel.SUMIF;
import org.boris.expr.function.excel.SUMPRODUCT;
import org.boris.expr.function.excel.SUMSQ;
import org.boris.expr.function.excel.SUMX2MY2;
import org.boris.expr.function.excel.SUMX2PY2;
import org.boris.expr.function.excel.SUMXMY2;
import org.boris.expr.function.excel.TAN;
import org.boris.expr.function.excel.TANH;
import org.boris.expr.function.excel.TRUNC;

public class ExcelMathAndTrigFunctionsTest extends TH
{
    public void testABS() throws Exception {
        ABS a = new ABS();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000 - Math.random() * 1000;
            assertEquals("ABS not working", TH.eval(a, d), Math.abs(d));
        }
    }

    public void testACOS() throws Exception {
        ACOS a = new ACOS();
        TH.testDoubleInOutFunction(a);
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * Math.PI;
            assertEquals("ACOS not working", TH.eval(a, d), Math.acos(d));
        }
    }

    public void testACOSH() throws Exception {
        ACOSH a = new ACOSH();
        assertEquals(TH.eval(a, 1), 0.);
        assertEquals(TH.eval(a, 10), 2.993222846126);
    }

    public void testASIN() throws Exception {
        ASIN a = new ASIN();
        TH.testDoubleInOutFunction(a);
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * Math.PI;
            assertEquals("ASIN not working", TH.eval(a, d), Math.asin(d));
        }
    }

    public void testASINH() throws Exception {
        ASINH a = new ASINH();
        assertEquals(TH.eval(a, 2.5), 1.647231146371);
        assertEquals(TH.eval(a, 10), 2.99822295029);
    }

    public void testATAN() throws Exception {
        ATAN a = new ATAN();
        TH.testDoubleInOutFunction(a);
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * Math.PI;
            assertEquals("ATAN not working", TH.eval(a, d), Math.atan(d));
        }
    }

    public void testATAN2() throws Exception {
        ATAN2 a = new ATAN2();
        assertEquals(TH.eval(a, 1, 1), 0.785398163397448);
        assertEquals(TH.eval(a, -1, -1), -2.35619449019234);
    }

    public void testATANH() throws Exception {
        ATANH a = new ATANH();
        assertEquals(TH.eval(a, 0.76159416), 1.00000000962971);
        assertEquals(TH.eval(a, -0.1), -0.10033534773);
    }

    public void testCEILING() throws Exception {
        CEILING c = new CEILING();
        assertEquals(TH.eval(c, 2.5, 1), 3.);
        assertEquals(TH.eval(c, -2.5, -2), -2.);
        assertEquals(TH.eval(c, -2.5, 2), ExprError.NUM);
        assertEquals(TH.eval(c, 1.5, 0.1), 1.5);
        assertEquals(TH.eval(c, 0.234, 0.01), 0.24);
    }

    public void testCOMBIN() throws Exception {
        COMBIN c = new COMBIN();
        assertEquals(eval(c, 8, 2), 28.);
        assertEquals(eval(c, 9, 7), 36.);
    }

    public void testCOS() throws Exception {
        COS c = new COS();
        TH.testDoubleInOutFunction(c);
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000 - Math.random() * 1000;
            assertEquals("COS not working", TH.eval(c, d), Math.cos(d));
        }
    }

    public void testCOSH() throws Exception {
        COSH c = new COSH();
        TH.testDoubleInOutFunction(c);
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000 - Math.random() * 1000;
            assertEquals("COS not working", TH.eval(c, d), Math.cosh(d));
        }
    }

    public void testDEGREES() throws Exception {
        DEGREES d = new DEGREES();
        assertEquals(eval(d, Math.PI), 180.);
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

    public void testEXP() throws Exception {
        EXP e = new EXP();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 10;
            assertEquals(eval(e, d), Math.exp(d));
        }
    }

    public void testFACT() throws Exception {
        FACT f = new FACT();
        assertEquals(eval(f, 0), 1.);
        assertEquals(eval(f, 1), 1.);
        assertEquals(eval(f, 2), 2.);
        assertEquals(eval(f, 3), 6.);
        assertEquals(eval(f, 5), 120.);
        assertEquals(eval(f, 1.9), 1.);
        assertEquals(eval(f, -1), ExprError.NUM);
    }

    public void testFLOOR() throws Exception {
        assertResult("FLOOR(2.5, 1)", 2.);
        assertResult("FLOOR(-2.5, -2)", -2.);
        assertResult("FLOOR(-2.5, 2)", ExprError.NUM);
        assertResult("FLOOR(1.5, 0.1)", 1.5);
        assertResult("FLOOR(0.234, 0.01)", 0.23);
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

    public void testLN() throws Exception {
        LN l = new LN();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000;
            assertEquals("LN not working", eval(l, d), Math.log(d));
        }
    }

    public void testLOG() throws Exception {
        LOG l = new LOG();
        assertEquals(eval(l, 86, Math.E), 4.45434734288829);
        assertEquals(eval(l, 10, 2), 3.32192809488736);
        assertEquals(eval(l, 10, 1), 1.);
        assertEquals(eval(l, 8, 2), 3.);
    }

    public void testLOG10() throws Exception {
        LOG10 l = new LOG10();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000;
            assertEquals("LOG10 not working", eval(l, d), Math.log10(d));
        }
    }

    public void testMDETERM() throws Exception {
        MDETERM m = new MDETERM();
        fail("MDETERM not implemented");
    }

    public void testMINVERSE() throws Exception {
        MINVERSE m = new MINVERSE();
        fail("MINVERSE not implemented");
    }

    public void testMMULT() throws Exception {
        MMULT m = new MMULT();
        fail("MMULT not implemented");
    }

    public void testMOD() throws Exception {
        MOD m = new MOD();
        assertEquals(eval(m, 3, 2), 1.);
        assertEquals(eval(m, -3, 2), 1.);
        assertEquals(eval(m, 3, -2), -1.);
        assertEquals(eval(m, -3, -2), -1.);
    }

    public void testODD() throws Exception {
        ODD o = new ODD();
        assertEquals(eval(o, 1.5), 3.);
        assertEquals(eval(o, 3), 3.);
        assertEquals(eval(o, 2), 3.);
        assertEquals(eval(o, -1), -1.);
        assertEquals(eval(o, -2), -3.);
    }

    public void testPI() throws Exception {
        PI p = new PI();
        assertEquals(eval(p), Math.PI);
        assertException("pi(23)");
    }

    public void testPOWER() throws Exception {
        POWER p = new POWER();
        for (int i = 0; i < 100; i++) {
            double d1 = Math.random() * 100;
            double d2 = Math.random() * 5 - Math.random() * 5;
            assertEquals(eval(p, d1, d2), Math.pow(d1, d2));
        }
    }

    public void testPRODUCT() throws Exception {
        assertResult("product({5,15,30})", 2250.);
        assertResult("product({5,15,30},2)", 4500.);
        assertResult("product(,,,3)", 3.);
        assertResult("product(c3,,,2,2,5.)", 20.);
    }

    public void testRADIANS() throws Exception {
        RADIANS r = new RADIANS();
        assertEquals(eval(r, 270), 4.71238898038469);
    }

    public void testRAND() throws Exception {
        RAND r = new RAND();
        assertNotSame(eval(r), eval(r));
    }

    public void testROMAN() throws Exception {
        ROMAN r = new ROMAN();
        fail("ROMAN not implemented");
    }

    public void testROUND() throws Exception {
        assertResult("ROUND(2.15, 1)", 2.2);
        assertResult("ROUND(2.149, 1)", 2.1);
        assertResult("ROUND(21.5, -1)", 20.);
        assertResult("ROUND(-1.475, 2)", -1.48);
    }

    public void testROUNDDOWN() throws Exception {
        assertResult("ROUNDDOWN(3.2, 0)", 3.);
        assertResult("ROUNDDOWN(76.9,0)", 76.);
        assertResult("ROUNDDOWN(3.14159, 3)", 3.141);
        assertResult("ROUNDDOWN(-3.14159, 1)", -3.1);
        assertResult("ROUNDDOWN(31415.92654, -2)", 31400.);
    }

    public void testROUNDUP() throws Exception {
        ROUNDUP r = new ROUNDUP();
        assertEquals(eval(r, 3.2, 0), 4.);
        assertEquals(eval(r, 76.9, 0), 77.);
        assertEquals(eval(r, 3.14159, 3), 3.142);
        assertEquals(eval(r, -3.14159, 1), -3.2);
        assertEquals(eval(r, 31415.92654, -2), 31500.);
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

    public void testSQRT() throws Exception {
        SQRT s = new SQRT();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000;
            assertEquals("SQRT not working", eval(s, d), Math.sqrt(d));
        }
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
        assertEquals(eval(s, toArray(2, 3, 9, 1, 8, 7, 5), toArray(6, 5, 11, 7,
                5, 4, 4)), -55.);
    }

    public void testSUMX2PY2() throws Exception {
        SUMX2PY2 s = new SUMX2PY2();
        assertEquals(eval(s, toArray(2, 3, 9, 1, 8, 7, 5), toArray(6, 5, 11, 7,
                5, 4, 4)), 521.);
    }

    public void testSUMXMY2() throws Exception {
        SUMXMY2 s = new SUMXMY2();
        assertEquals(eval(s, toArray(2, 3, 9, 1, 8, 7, 5), toArray(6, 5, 11, 7,
                5, 4, 4)), 79.);
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

    public void testTRUNC() throws Exception {
        TRUNC t = new TRUNC();
        assertEquals(eval(t, 8.9, 3), 8.9);
        assertEquals(eval(t, 8.9), 8.);
        assertEquals(eval(t, -8.9), -8.);
        assertEquals(eval(t, Math.PI), 3.);
    }
}
