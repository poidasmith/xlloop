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

import org.boris.expr.function.excel.ABS;
import org.boris.expr.function.excel.ACOS;
import org.boris.expr.function.excel.ACOSH;
import org.boris.expr.function.excel.ADDRESS;
import org.boris.expr.function.excel.AND;
import org.boris.expr.function.excel.ASIN;
import org.boris.expr.function.excel.ASINH;
import org.boris.expr.function.excel.ATAN;
import org.boris.expr.function.excel.ATAN2;
import org.boris.expr.function.excel.ATANH;
import org.boris.expr.function.excel.AVEDEV;
import org.boris.expr.function.excel.AVERAGE;
import org.boris.expr.function.excel.BAHTTEXT;
import org.boris.expr.function.excel.BETADIST;
import org.boris.expr.function.excel.BETAINV;
import org.boris.expr.function.excel.BINOMDIST;
import org.boris.expr.function.excel.CEILING;
import org.boris.expr.function.excel.CELL;
import org.boris.expr.function.excel.CHIDIST;
import org.boris.expr.function.excel.CHIINV;
import org.boris.expr.function.excel.CHITEST;
import org.boris.expr.function.excel.CHOOSE;
import org.boris.expr.function.excel.CLEAN;
import org.boris.expr.function.excel.CODE;
import org.boris.expr.function.excel.COMBIN;
import org.boris.expr.function.excel.CONCATENATE;
import org.boris.expr.function.excel.CONFIDENCE;
import org.boris.expr.function.excel.CORREL;
import org.boris.expr.function.excel.COS;
import org.boris.expr.function.excel.COSH;
import org.boris.expr.function.excel.COUNT;
import org.boris.expr.function.excel.COUNTA;
import org.boris.expr.function.excel.COUNTBLANK;
import org.boris.expr.function.excel.COUNTIF;
import org.boris.expr.function.excel.COVAR;
import org.boris.expr.function.excel.CRITBINOM;
import org.boris.expr.function.excel.DATE;
import org.boris.expr.function.excel.DATEVALUE;
import org.boris.expr.function.excel.DAVERAGE;
import org.boris.expr.function.excel.DAY;
import org.boris.expr.function.excel.DB;
import org.boris.expr.function.excel.DCOUNT;
import org.boris.expr.function.excel.DCOUNTA;
import org.boris.expr.function.excel.DDB;
import org.boris.expr.function.excel.DEGREES;
import org.boris.expr.function.excel.DEVSQ;
import org.boris.expr.function.excel.DGET;
import org.boris.expr.function.excel.DMAX;
import org.boris.expr.function.excel.DMIN;
import org.boris.expr.function.excel.DOLLAR;
import org.boris.expr.function.excel.DPRODUCT;
import org.boris.expr.function.excel.DSTDEV;
import org.boris.expr.function.excel.DSTDEVP;
import org.boris.expr.function.excel.DSUM;
import org.boris.expr.function.excel.DVAR;
import org.boris.expr.function.excel.DVARP;

public class ExcelFunctionTestAtoD extends TH
{
    public void testABS() throws Exception {
        ABS a = new ABS();
        TH.testDoubleInOutFunction(a);
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
        assertEquals(TH.eval(a, 10), 2.993222846);
    }

    public void testADDRESS() throws Exception {
        ADDRESS a = new ADDRESS();
        assertEquals(TH.eval(a, 1, 1, 2, 0, "Sheet "), "\'Sheet \'!R1C[1]");
        assertEquals(TH.eval(a, 2, 3), "$C$2");
        assertEquals(TH.eval(a, 2, 3, 2), "C$2");
        assertEquals(TH.eval(a, 2, 3, 2, false), "R2C[3]");
        assertEquals(TH.eval(a, 2, 3, 1, false, "EXCEL SHEET"),
                "\'EXCEL SHEET\'!R2C3");
    }

    public void testAND() throws Exception {
        AND a = new AND();
        assertEquals(TH.eval(a, 3, true, 3.4), true);
        assertEquals(TH.eval(a, 3, false, 3.4), false);
        TH.assertException(a);
        TH.assertException(a, "asdf");
    }

    public void testAREAS() throws Exception {
        TH.assertResult("AREAS(B4:C7)", 1);
        TH.assertResult("AREAS(B2:D4)", 1);
        TH.assertResult("AREAS((B2:D4,E5,F6:I9))", 3);
        TH.assertResult("AREAS(B2:D4 B2)", 1);
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
        assertEquals(TH.eval(a, 2.5), -1.647231146);
        assertEquals(TH.eval(a, 10), 2.99822295);
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
        assertEquals(TH.eval(a, 1, 1), 0.785398163);
        assertEquals(TH.eval(a, -1, -1), -2.35619449);
    }

    public void testATANH() throws Exception {
        ATANH a = new ATANH();
        assertEquals(TH.eval(a, 0.76159416), 1.00000001);
        assertEquals(TH.eval(a, -0.1), -0.100335348);
    }

    public void testAVEDEV() throws Exception {
        AVEDEV a = new AVEDEV();
        assertEquals(TH.eval(a, 1, 2, 3, 4, 5), 1.2);
        TH.assertEquals((Double) TH.eval(a, 4, 5, 6, 7, 5, 4, 3), 1.020408163);
    }

    public void testAVERAGE() throws Exception {
        AVERAGE a = new AVERAGE();
        assertEquals(TH.eval(a, 23, 23, 23), 23.);
        assertEquals(TH.eval(a, ExprMissing.MISSING), ExprError.DIV0);
    }

    public void testBAHTTEXT() throws Exception {
        BAHTTEXT b = new BAHTTEXT();
        fail("BAHTTEXT not implemented");
    }

    public void testBETADIST() throws Exception {
        BETADIST b = new BETADIST();
        fail("BETADIST not implemented");
    }

    public void testBETAINV() throws Exception {
        BETAINV b = new BETAINV();
        fail("BETAINV not implemented");
    }

    public void testBINOMDIST() throws Exception {
        BINOMDIST b = new BINOMDIST();
        fail("BINOMDIST not implemented");
    }

    public void testCEILING() throws Exception {
        CEILING c = new CEILING();
        assertEquals(TH.eval(c, 2.5, 1), 3.);
        assertEquals(TH.eval(c, -2.5, -2), -2.);
        assertEquals(TH.eval(c, -2.5, 2), ExprError.NUM);
        assertEquals(TH.eval(c, 1.5, 0.1), 1.5);
        assertEquals(TH.eval(c, 0.234, 0.01), 0.24);
    }

    public void testCELL() throws Exception {
        CELL c = new CELL();
        fail("CELL not implemented");
    }

    public void testCHIDIST() throws Exception {
        CHIDIST c = new CHIDIST();
        fail("CHIDIST not implemented");
    }

    public void testCHIINV() throws Exception {
        CHIINV c = new CHIINV();
        fail("CHIINV not implemented");
    }

    public void testCHITEST() throws Exception {
        CHITEST c = new CHITEST();
        fail("CHITEST not implemented");
    }

    public void testCHOOSE() throws Exception {
        CHOOSE c = new CHOOSE();
        assertEquals(TH.eval(c, 1, 3, 4), 3);
        assertEquals(TH.eval(c, 2, 3, "hello"), "hello");
        assertEquals(TH.eval(c, true, 35), 35);
        TH.assertException(c, 1.2);
        TH.assertException(c, 1);
        TH.assertException(c, "asdf");
        TH.assertException(c, 0, 23, 34);
        TH.assertException(c);
    }

    public void testCLEAN() throws Exception {
        CLEAN c = new CLEAN();
        fail("CLEAN not implemented");
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

    public void testCOMBIN() throws Exception {
        COMBIN c = new COMBIN();
        fail("COMBIN not implemented");
    }

    public void testCONCATENATE() throws Exception {
        CONCATENATE c = new CONCATENATE();
        assertEquals(eval(c, "asdf"), "asdf");
        assertEquals(eval(c, "asdf", "qwer"), "asdfqwer");
        assertEquals(eval(c, 1, 2, 3.), "123.0");
    }

    public void testCONFIDENCE() throws Exception {
        CONFIDENCE c = new CONFIDENCE();
        fail("CONFIDENCE not implemented");
    }

    public void testCORREL() throws Exception {
        CORREL c = new CORREL();
        fail("CORREL not implemented");
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

    public void testCOUNT() throws Exception {
        COUNT c = new COUNT();
        assertEquals(TH.eval(c, 1, 2, 3, "asdf", true), 3);
        assertEquals(TH.eval(c, (Object) null, ExprMissing.MISSING), 0);
        assertEquals(TH.eval(c, 1.2, -1.2), 2);
    }

    public void testCOUNTA() throws Exception {
        COUNTA c = new COUNTA();
        assertEquals(TH.eval(c, 1, 2, 3, "asdf", true), 5);
        assertEquals(TH.eval(c, (Object) null, ExprMissing.MISSING), 0);
        assertEquals(TH.eval(c, 1.2, -1.2), 2);
    }

    public void testCOUNTBLANK() throws Exception {
        COUNTBLANK c = new COUNTBLANK();
        assertEquals(TH.eval(c, ExprMissing.MISSING, 1), 1);
        assertEquals(TH.eval(c, (Object) ExprMissing.MISSING), 1);
        assertEquals(TH.eval(c, 1, 2, 3, "asdf", true), 0);
        assertEquals(TH.eval(c, (Object) null, ExprMissing.MISSING), 1);
        assertEquals(TH.eval(c, 1.2, -1.2), 0);
    }

    public void testCOUNTIF() throws Exception {
        COUNTIF c = new COUNTIF();
        fail("COUNTIF not implemented");
    }

    public void testCOVAR() throws Exception {
        COVAR c = new COVAR();
        fail("COVAR not implemented");
    }

    public void testCRITBINOM() throws Exception {
        CRITBINOM c = new CRITBINOM();
        fail("CRITBINOM not implemented");
    }

    public void testDATE() throws Exception {
        DATE d = new DATE();
        fail("DATE not implemented");
    }

    public void testDATEVALUE() throws Exception {
        DATEVALUE d = new DATEVALUE();
        fail("DATEVALUE not implemented");
    }

    public void testDAVERAGE() throws Exception {
        DAVERAGE d = new DAVERAGE();
        fail("DAVERAGE not implemented");
    }

    public void testDAY() throws Exception {
        DAY d = new DAY();
        fail("DAY not implemented");
    }

    public void testDB() throws Exception {
        DB d = new DB();
        fail("DB not implemented");
    }

    public void testDCOUNT() throws Exception {
        DCOUNT d = new DCOUNT();
        fail("DCOUNT not implemented");
    }

    public void testDCOUNTA() throws Exception {
        DCOUNTA d = new DCOUNTA();
        fail("DCOUNTA not implemented");
    }

    public void testDDB() throws Exception {
        DDB d = new DDB();
        fail("DDB not implemented");
    }

    public void testDEGREES() throws Exception {
        DEGREES d = new DEGREES();
        fail("DEGREES not implemented");
    }

    public void testDEVSQ() throws Exception {
        DEVSQ d = new DEVSQ();
        fail("DEVSQ not implemented");
    }

    public void testDGET() throws Exception {
        DGET d = new DGET();
        fail("DGET not implemented");
    }

    public void testDMAX() throws Exception {
        DMAX d = new DMAX();
        fail("DMAX not implemented");
    }

    public void testDMIN() throws Exception {
        DMIN d = new DMIN();
        fail("DMIN not implemented");
    }

    public void testDOLLAR() throws Exception {
        DOLLAR d = new DOLLAR();
        fail("DOLLAR not implemented");
    }

    public void testDPRODUCT() throws Exception {
        DPRODUCT d = new DPRODUCT();
        fail("DPRODUCT not implemented");
    }

    public void testDSTDEV() throws Exception {
        DSTDEV d = new DSTDEV();
        fail("DSTDEV not implemented");
    }

    public void testDSTDEVP() throws Exception {
        DSTDEVP d = new DSTDEVP();
        fail("DSTDEVP not implemented");
    }

    public void testDSUM() throws Exception {
        DSUM d = new DSUM();
        fail("DSUM not implemented");
    }

    public void testDVAR() throws Exception {
        DVAR d = new DVAR();
        fail("DVAR not implemented");
    }

    public void testDVARP() throws Exception {
        DVARP d = new DVARP();
        fail("DVARP not implemented");
    }
}
