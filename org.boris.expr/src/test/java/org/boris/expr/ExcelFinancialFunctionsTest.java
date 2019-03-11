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

import org.boris.expr.function.excel.DB;
import org.boris.expr.function.excel.DDB;
import org.boris.expr.function.excel.FV;
import org.boris.expr.function.excel.IPMT;
import org.boris.expr.function.excel.IRR;
import org.boris.expr.function.excel.ISPMT;
import org.boris.expr.function.excel.MIRR;
import org.boris.expr.function.excel.NPER;
import org.boris.expr.function.excel.NPV;
import org.boris.expr.function.excel.PMT;
import org.boris.expr.function.excel.PPMT;
import org.boris.expr.function.excel.PV;
import org.boris.expr.function.excel.RATE;
import org.boris.expr.function.excel.SLN;
import org.boris.expr.function.excel.SYD;
import org.boris.expr.function.excel.VDB;

public class ExcelFinancialFunctionsTest extends TH
{
    public void testDB() throws Exception {
        DB d = new DB();
        assertEquals(eval(d, 1000000, 100000, 6, 1, 7), 186083.33333333334);
        assertEquals(eval(d, 1000000, 100000, 6, 2, 7), 259639.41666666667);
        assertEquals(eval(d, 1000000, 100000, 6, 3, 7), 176814.44275000002);
        assertEquals(eval(d, 1000000, 100000, 6, 4, 7), 120410.63551274998);
        assertEquals(eval(d, 1000000, 100000, 6, 5, 7), 81999.64278418274);
        assertEquals(eval(d, 1000000, 100000, 6, 6, 7), 55841.75673602846);
        assertEquals(eval(d, 1000000, 100000, 6, 7, 7), 15845.098473848071);
    }

    public void testDDB() throws Exception {
        DDB d = new DDB();
        assertEquals(eval(d, 2400, 300, 10 * 365, 1), 1.31506849315068);
        assertEquals(eval(d, 2400, 300, 10 * 12, 1, 2), 40.);
        assertEquals(eval(d, 2400, 300, 10, 1, 2), 480.);
        assertEquals(eval(d, 2400, 300, 10, 2, 1.5), 306.);
        assertEquals(eval(d, 2400, 300, 10, 10), 22.1225472000003);
    }

    public void testFV() throws Exception {
        FV f = new FV();
        fail("FV not implemented");
    }

    public void testIPMT() throws Exception {
        IPMT i = new IPMT();
        fail("IPMT not implemented");
    }

    public void testIRR() throws Exception {
        IRR i = new IRR();
        fail("IRR not implemented");
    }

    public void testISPMT() throws Exception {
        ISPMT i = new ISPMT();
        fail("ISPMT not implemented");
    }

    public void testMIRR() throws Exception {
        MIRR m = new MIRR();
        fail("MIRR not implemented");
    }

    public void testNPER() throws Exception {
        NPER n = new NPER();
        fail("NPER not implemented");
    }

    public void testNPV() throws Exception {
        NPV n = new NPV();
        fail("NPV not implemented");
    }

    public void testPMT() throws Exception {
        PMT p = new PMT();
        fail("PMT not implemented");
    }

    public void testPPMT() throws Exception {
        PPMT p = new PPMT();
        fail("PPMT not implemented");
    }

    public void testPV() throws Exception {
        PV p = new PV();
        fail("PV not implemented");
    }

    public void testRATE() throws Exception {
        RATE r = new RATE();
        fail("RATE not implemented");
    }

    public void testSLN() throws Exception {
        SLN s = new SLN();
        fail("SLN not implemented");
    }

    public void testSYD() throws Exception {
        SYD s = new SYD();
        assertEquals(eval(s, 30000, 7500, 10, 1), 4090.90909090909);
        assertEquals(eval(s, 30000, 7500, 10, 10), 409.090909090909);
    }

    public void testVDB() throws Exception {
        VDB v = new VDB();
        assertEquals(eval(v, 2400, 300, 10 * 365, 0, 1), 1.315068493150680);
        assertEquals(eval(v, 2400, 300, 10 * 12, 0, 1), 40.);
        assertEquals(eval(v, 2400, 300, 10, 0, 1), 480.);
        assertEquals(eval(v, 2400, 300, 10 * 12, 6, 18), 396.306053264751);
        assertEquals(eval(v, 2400, 300, 10 * 12, 6, 18, 1.5), 311.808936658234);
        assertEquals(eval(v, 2400, 300, 10, 0, 0.875, 1.5), 315.);
    }
}
