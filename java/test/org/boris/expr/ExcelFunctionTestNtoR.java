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

import org.boris.expr.function.excel.N;
import org.boris.expr.function.excel.NA;
import org.boris.expr.function.excel.NEGBINOMDIST;
import org.boris.expr.function.excel.NORMDIST;
import org.boris.expr.function.excel.NORMINV;
import org.boris.expr.function.excel.NORMSDIST;
import org.boris.expr.function.excel.NORMSINV;
import org.boris.expr.function.excel.NOT;
import org.boris.expr.function.excel.NOW;
import org.boris.expr.function.excel.NPER;
import org.boris.expr.function.excel.NPV;
import org.boris.expr.function.excel.ODD;
import org.boris.expr.function.excel.OFFSET;
import org.boris.expr.function.excel.OR;
import org.boris.expr.function.excel.PEARSON;
import org.boris.expr.function.excel.PERCENTILE;
import org.boris.expr.function.excel.PERCENTRANK;
import org.boris.expr.function.excel.PERMUT;
import org.boris.expr.function.excel.PI;
import org.boris.expr.function.excel.PMT;
import org.boris.expr.function.excel.POISSON;
import org.boris.expr.function.excel.POWER;
import org.boris.expr.function.excel.PPMT;
import org.boris.expr.function.excel.PROB;
import org.boris.expr.function.excel.PRODUCT;
import org.boris.expr.function.excel.PROPER;
import org.boris.expr.function.excel.PV;
import org.boris.expr.function.excel.QUARTILE;
import org.boris.expr.function.excel.RADIANS;
import org.boris.expr.function.excel.RAND;
import org.boris.expr.function.excel.RANK;
import org.boris.expr.function.excel.RATE;
import org.boris.expr.function.excel.REPLACE;
import org.boris.expr.function.excel.REPT;
import org.boris.expr.function.excel.RIIGHT;
import org.boris.expr.function.excel.ROMAN;
import org.boris.expr.function.excel.ROUND;
import org.boris.expr.function.excel.ROUNDDOWN;
import org.boris.expr.function.excel.ROUNDUP;
import org.boris.expr.function.excel.ROW;
import org.boris.expr.function.excel.ROWS;
import org.boris.expr.function.excel.RSQ;
import org.boris.expr.function.excel.RTD;

public class ExcelFunctionTestNtoR extends TH
{
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

    public void testNEGBINOMDIST() throws Exception {
        NEGBINOMDIST n = new NEGBINOMDIST();
        fail("NEGBINOMDIST not implemented");
    }

    public void testNORMDIST() throws Exception {
        NORMDIST n = new NORMDIST();
        fail("NORMDIST not implemented");
    }

    public void testNORMINV() throws Exception {
        NORMINV n = new NORMINV();
        fail("NORMINV not implemented");
    }

    public void testNORMSDIST() throws Exception {
        NORMSDIST n = new NORMSDIST();
        fail("NORMSDIST not implemented");
    }

    public void testNORMSINV() throws Exception {
        NORMSINV n = new NORMSINV();
        fail("NORMSINV not implemented");
    }

    public void testNOT() throws Exception {
        NOT n = new NOT();
        assertEquals(eval(n, true), false);
        assertEquals(eval(n, 1), false);
        assertEquals(eval(n, 0), true);
        assertEquals(eval(n, -10.3), false);
        assertException("not(123,23)");
    }

    public void testNOW() throws Exception {
        NOW n = new NOW();
        fail("NOW not implemented");
    }

    public void testNPER() throws Exception {
        NPER n = new NPER();
        fail("NPER not implemented");
    }

    public void testNPV() throws Exception {
        NPV n = new NPV();
        fail("NPV not implemented");
    }

    public void testODD() throws Exception {
        ODD o = new ODD();
        fail("ODD not implemented");
    }

    public void testOFFSET() throws Exception {
        OFFSET o = new OFFSET();
        fail("OFFSET not implemented");
    }

    public void testOR() throws Exception {
        OR o = new OR();
        fail("OR not implemented");
    }

    public void testPEARSON() throws Exception {
        PEARSON p = new PEARSON();
        fail("PEARSON not implemented");
    }

    public void testPERCENTILE() throws Exception {
        PERCENTILE p = new PERCENTILE();
        fail("PERCENTILE not implemented");
    }

    public void testPERCENTRANK() throws Exception {
        PERCENTRANK p = new PERCENTRANK();
        fail("PERCENTRANK not implemented");
    }

    public void testPERMUT() throws Exception {
        PERMUT p = new PERMUT();
        fail("PERMUT not implemented");
    }

    public void testPI() throws Exception {
        PI p = new PI();
        fail("PI not implemented");
    }

    public void testPMT() throws Exception {
        PMT p = new PMT();
        fail("PMT not implemented");
    }

    public void testPOISSON() throws Exception {
        POISSON p = new POISSON();
        fail("POISSON not implemented");
    }

    public void testPOWER() throws Exception {
        POWER p = new POWER();
        fail("POWER not implemented");
    }

    public void testPPMT() throws Exception {
        PPMT p = new PPMT();
        fail("PPMT not implemented");
    }

    public void testPROB() throws Exception {
        PROB p = new PROB();
        fail("PROB not implemented");
    }

    public void testPRODUCT() throws Exception {
        PRODUCT p = new PRODUCT();
        fail("PRODUCT not implemented");
    }

    public void testPROPER() throws Exception {
        PROPER p = new PROPER();
        fail("PROPER not implemented");
    }

    public void testPV() throws Exception {
        PV p = new PV();
        fail("PV not implemented");
    }

    public void testQUARTILE() throws Exception {
        QUARTILE q = new QUARTILE();
        fail("QUARTILE not implemented");
    }

    public void testRADIANS() throws Exception {
        RADIANS r = new RADIANS();
        fail("RADIANS not implemented");
    }

    public void testRAND() throws Exception {
        RAND r = new RAND();
        fail("RAND not implemented");
    }

    public void testRANK() throws Exception {
        RANK r = new RANK();
        fail("RANK not implemented");
    }

    public void testRATE() throws Exception {
        RATE r = new RATE();
        fail("RATE not implemented");
    }

    public void testREPLACE() throws Exception {
        REPLACE r = new REPLACE();
        fail("REPLACE not implemented");
    }

    public void testREPT() throws Exception {
        REPT r = new REPT();
        fail("REPT not implemented");
    }

    public void testRIIGHT() throws Exception {
        RIIGHT r = new RIIGHT();
        fail("RIIGHT not implemented");
    }

    public void testROMAN() throws Exception {
        ROMAN r = new ROMAN();
        fail("ROMAN not implemented");
    }

    public void testROUND() throws Exception {
        ROUND r = new ROUND();
        fail("ROUND not implemented");
    }

    public void testROUNDDOWN() throws Exception {
        ROUNDDOWN r = new ROUNDDOWN();
        fail("ROUNDDOWN not implemented");
    }

    public void testROUNDUP() throws Exception {
        ROUNDUP r = new ROUNDUP();
        fail("ROUNDUP not implemented");
    }

    public void testROW() throws Exception {
        ROW r = new ROW();
        fail("ROW not implemented");
    }

    public void testROWS() throws Exception {
        ROWS r = new ROWS();
        fail("ROWS not implemented");
    }

    public void testRSQ() throws Exception {
        RSQ r = new RSQ();
        fail("RSQ not implemented");
    }

    public void testRTD() throws Exception {
        RTD r = new RTD();
        fail("RTD not implemented");
    }
}
