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

import org.boris.expr.function.excel.KURT;
import org.boris.expr.function.excel.LARGE;
import org.boris.expr.function.excel.LEFT;
import org.boris.expr.function.excel.LEN;
import org.boris.expr.function.excel.LINEST;
import org.boris.expr.function.excel.LN;
import org.boris.expr.function.excel.LOG;
import org.boris.expr.function.excel.LOG10;
import org.boris.expr.function.excel.LOGEST;
import org.boris.expr.function.excel.LOGINV;
import org.boris.expr.function.excel.LOGNORMDIST;
import org.boris.expr.function.excel.LOOKUP;
import org.boris.expr.function.excel.LOWER;
import org.boris.expr.function.excel.MATCH;
import org.boris.expr.function.excel.MAX;
import org.boris.expr.function.excel.MAXA;
import org.boris.expr.function.excel.MDETERM;
import org.boris.expr.function.excel.MEDIAN;
import org.boris.expr.function.excel.MID;
import org.boris.expr.function.excel.MIN;
import org.boris.expr.function.excel.MINA;
import org.boris.expr.function.excel.MINUTE;
import org.boris.expr.function.excel.MINVERSE;
import org.boris.expr.function.excel.MIRR;
import org.boris.expr.function.excel.MMULT;
import org.boris.expr.function.excel.MOD;
import org.boris.expr.function.excel.MODE;
import org.boris.expr.function.excel.MONTH;

public class ExcelFunctionTestKtoM extends TH
{
    public void testKURT() throws Exception {
        KURT k = new KURT();
        fail("KURT not implemented");
    }

    public void testLARGE() throws Exception {
        LARGE l = new LARGE();
        fail("LARGE not implemented");
    }

    public void testLEFT() throws Exception {
        LEFT l = new LEFT();
        fail("LEFT not implemented");
    }

    public void testLEN() throws Exception {
        LEN l = new LEN();
        fail("LEN not implemented");
    }

    public void testLINEST() throws Exception {
        LINEST l = new LINEST();
        fail("LINEST not implemented");
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

        fail("LOG not implemented");
    }

    public void testLOG10() throws Exception {
        LOG10 l = new LOG10();
        for (int i = 0; i < 100; i++) {
            double d = Math.random() * 1000;
            assertEquals("LOG10 not working", eval(l, d), Math.log10(d));
        }
    }

    public void testLOGEST() throws Exception {
        LOGEST l = new LOGEST();
        fail("LOGEST not implemented");
    }

    public void testLOGINV() throws Exception {
        LOGINV l = new LOGINV();
        fail("LOGINV not implemented");
    }

    public void testLOGNORMDIST() throws Exception {
        LOGNORMDIST l = new LOGNORMDIST();
        fail("LOGNORMDIST not implemented");
    }

    public void testLOOKUP() throws Exception {
        LOOKUP l = new LOOKUP();
        fail("LOOKUP not implemented");
    }

    public void testLOWER() throws Exception {
        LOWER l = new LOWER();
        fail("LOWER not implemented");
    }

    public void testMATCH() throws Exception {
        MATCH m = new MATCH();
        fail("MATCH not implemented");
    }

    public void testMAX() throws Exception {
        MAX m = new MAX();
        fail("MAX not implemented");
    }

    public void testMAXA() throws Exception {
        MAXA m = new MAXA();
        fail("MAXA not implemented");
    }

    public void testMDETERM() throws Exception {
        MDETERM m = new MDETERM();
        fail("MDETERM not implemented");
    }

    public void testMEDIAN() throws Exception {
        MEDIAN m = new MEDIAN();
        fail("MEDIAN not implemented");
    }

    public void testMID() throws Exception {
        MID m = new MID();
        fail("MID not implemented");
    }

    public void testMIN() throws Exception {
        MIN m = new MIN();
        fail("MIN not implemented");
    }

    public void testMINA() throws Exception {
        MINA m = new MINA();
        fail("MINA not implemented");
    }

    public void testMINUTE() throws Exception {
        MINUTE m = new MINUTE();
        fail("MINUTE not implemented");
    }

    public void testMINVERSE() throws Exception {
        MINVERSE m = new MINVERSE();
        fail("MINVERSE not implemented");
    }

    public void testMIRR() throws Exception {
        MIRR m = new MIRR();
        fail("MIRR not implemented");
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

    public void testMODE() throws Exception {
        MODE m = new MODE();
        fail("MODE not implemented");
    }

    public void testMONTH() throws Exception {
        MONTH m = new MONTH();
        fail("MONTH not implemented");
    }
}
