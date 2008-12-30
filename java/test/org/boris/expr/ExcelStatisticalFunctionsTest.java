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
import org.boris.expr.function.excel.AVEDEV;
import org.boris.expr.function.excel.AVERAGE;
import org.boris.expr.function.excel.AVERAGEA;
import org.boris.expr.function.excel.BETADIST;
import org.boris.expr.function.excel.BETAINV;
import org.boris.expr.function.excel.BINOMDIST;
import org.boris.expr.function.excel.CHIDIST;
import org.boris.expr.function.excel.CHIINV;
import org.boris.expr.function.excel.CHITEST;
import org.boris.expr.function.excel.CONFIDENCE;
import org.boris.expr.function.excel.CORREL;
import org.boris.expr.function.excel.COUNT;
import org.boris.expr.function.excel.COUNTA;
import org.boris.expr.function.excel.COUNTBLANK;
import org.boris.expr.function.excel.COVAR;
import org.boris.expr.function.excel.CRITBINOM;
import org.boris.expr.function.excel.DEVSQ;
import org.boris.expr.function.excel.EXPONDIST;
import org.boris.expr.function.excel.FDIST;
import org.boris.expr.function.excel.FINV;
import org.boris.expr.function.excel.FISHER;
import org.boris.expr.function.excel.FISHERNV;
import org.boris.expr.function.excel.FORECAST;
import org.boris.expr.function.excel.FREQUENCY;
import org.boris.expr.function.excel.FTEST;
import org.boris.expr.function.excel.GAMMADIST;
import org.boris.expr.function.excel.GAMMAINV;
import org.boris.expr.function.excel.GAMMALN;
import org.boris.expr.function.excel.GEOMEAN;
import org.boris.expr.function.excel.GROWTH;
import org.boris.expr.function.excel.HARMEAN;
import org.boris.expr.function.excel.HYPGEOMDIST;
import org.boris.expr.function.excel.INTERCEPT;
import org.boris.expr.function.excel.KURT;
import org.boris.expr.function.excel.LARGE;
import org.boris.expr.function.excel.LINEST;
import org.boris.expr.function.excel.LOGEST;
import org.boris.expr.function.excel.LOGINV;
import org.boris.expr.function.excel.LOGNORMDIST;
import org.boris.expr.function.excel.MAX;
import org.boris.expr.function.excel.MAXA;
import org.boris.expr.function.excel.MEDIAN;
import org.boris.expr.function.excel.MIN;
import org.boris.expr.function.excel.MINA;
import org.boris.expr.function.excel.MODE;
import org.boris.expr.function.excel.NEGBINOMDIST;
import org.boris.expr.function.excel.NORMDIST;
import org.boris.expr.function.excel.NORMINV;
import org.boris.expr.function.excel.NORMSDIST;
import org.boris.expr.function.excel.NORMSINV;
import org.boris.expr.function.excel.PEARSON;
import org.boris.expr.function.excel.PERCENTILE;
import org.boris.expr.function.excel.PERCENTRANK;
import org.boris.expr.function.excel.PERMUT;
import org.boris.expr.function.excel.POISSON;
import org.boris.expr.function.excel.PROB;
import org.boris.expr.function.excel.QUARTILE;
import org.boris.expr.function.excel.RANK;
import org.boris.expr.function.excel.RSQ;
import org.boris.expr.function.excel.SKEW;
import org.boris.expr.function.excel.SLOPE;
import org.boris.expr.function.excel.SMALL;
import org.boris.expr.function.excel.STANDARDIZE;
import org.boris.expr.function.excel.STEYX;
import org.boris.expr.function.excel.TDIST;
import org.boris.expr.function.excel.TINV;
import org.boris.expr.function.excel.TREND;
import org.boris.expr.function.excel.TRIMMEAN;
import org.boris.expr.function.excel.TTEST;
import org.boris.expr.function.excel.WEIBULL;
import org.boris.expr.function.excel.ZTEST;

public class ExcelStatisticalFunctionsTest extends TH
{
    public void testAVEDEV() throws Exception {
        AVEDEV a = new AVEDEV();
        assertEquals(TH.eval(a, 1, 2, 3, 4, 5), 1.2);
        assertEquals(eval(a, 4, 5, 6, 7, 5, 4, 3), 1.02040816326531);
    }

    public void testAVERAGE() throws Exception {
        AVERAGE a = new AVERAGE();
        assertEquals(eval(a, 23, 23, 23), 23.);
        assertEquals(eval(a, ExprMissing.MISSING), ExprError.DIV0);
    }

    public void testAVERAGEA() throws Exception {
        AVERAGEA a = new AVERAGEA();
        assertEquals(eval(a, 10, 7, 9, 2, "Not available"), 5.6);
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
        assertEquals(eval(b, 6, 10, 0.5, false), 0.205078125);
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

    public void testCONFIDENCE() throws Exception {
        CONFIDENCE c = new CONFIDENCE();
        assertEquals(eval(c, 0.05, 2.5, 50), 0.692951912174839);
        assertEquals(eval(c, 0.07, 3.5, 2000), 0.141804440185779);
    }

    public void testCORREL() throws Exception {
        CORREL c = new CORREL();
        fail("CORREL not implemented");
    }

    public void testCOUNT() throws Exception {
        COUNT c = new COUNT();
        assertEquals(eval(c, 1, 2, 3, "asdf", true), 3);
        assertEquals(eval(c, (Object) null, ExprMissing.MISSING), 0);
        assertEquals(eval(c, 1.2, -1.2), 2);
    }

    public void testCOUNTA() throws Exception {
        COUNTA c = new COUNTA();
        assertEquals(eval(c, 1, 2, 3, "asdf", true), 5);
        assertEquals(eval(c, (Object) null, ExprMissing.MISSING), 0);
        assertEquals(eval(c, 1.2, -1.2), 2);
    }

    public void testCOUNTBLANK() throws Exception {
        COUNTBLANK c = new COUNTBLANK();
        assertEquals(eval(c, ExprMissing.MISSING, 1), 1);
        assertEquals(eval(c, (Object) ExprMissing.MISSING), 1);
        assertEquals(eval(c, 1, 2, 3, "asdf", true), 0);
        assertEquals(eval(c, (Object) null, ExprMissing.MISSING), 1);
        assertEquals(eval(c, 1.2, -1.2), 0);
    }

    public void testCOUNTIF() throws Exception {
        BasicEvaluationCallback c = new BasicEvaluationCallback();
        c.set(loadArray("countif1.txt"));
        assertResult(c, "COUNTIF(A1:A5,\"apples\")", 2.);
        assertResult(c, "COUNTIF(B2:B5,\">55\")", 2.);
    }

    public void testCOVAR() throws Exception {
        AbstractFunction c = new COVAR();
        assertEquals(
                eval(c, toArray(3, 2, 4, 5, 6), toArray(9, 7, 12, 15, 17)), 5.2);
        assertEquals(eval(c, toArray(3, ExprMissing.MISSING, 4, 5, 6), toArray(
                9, 7, 12, 15, 17)), 3.375);
    }

    public void testCRITBINOM() throws Exception {
        CRITBINOM c = new CRITBINOM();
        assertEquals(eval(c, 6, 0.5, 0.75), 4.);
    }

    public void testDEVSQ() throws Exception {
        DEVSQ d = new DEVSQ();
        assertEquals(eval(d, 4, 5, 8, 7, 11, 4, 3), 48.);
    }

    public void testEXPONDIST() throws Exception {
        EXPONDIST e = new EXPONDIST();
        assertEquals(eval(e, 0.2, 10, true), 0.864664716763387);
        assertEquals(eval(e, 0.2, 10, false), 1.35335283236613);
    }

    public void testFDIST() throws Exception {
        FDIST f = new FDIST();
        fail("FDIST not implemented");
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

    public void testFORECAST() throws Exception {
        FORECAST f = new FORECAST();
        assertEquals(eval(f, 30, toArray(6, 7, 9, 15, 21), toArray(20, 28, 31,
                38, 40)), 10.6072530864198);
    }

    public void testFREQUENCY() throws Exception {
        FREQUENCY f = new FREQUENCY();
        fail("FREQUENCY not implemented");
    }

    public void testFTEST() throws Exception {
        FTEST f = new FTEST();
        fail("FTEST not implemented");
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
        assertEquals(eval(g, 1, 2, 3, 4, 5, 6), 2.99379516552391);
    }

    public void testGROWTH() throws Exception {
        GROWTH g = new GROWTH();
        fail("GROWTH not implemented");
    }

    public void testHARMEAN() throws Exception {
        HARMEAN h = new HARMEAN();
        assertEquals(eval(h, 1, 2, 3, 4, 5, 6), 2.44897959183674);
        assertEquals(eval(h, 1, 2, 3, 0), ExprError.NUM);
    }

    public void testHYPGEOMDIST() throws Exception {
        HYPGEOMDIST h = new HYPGEOMDIST();
        fail("HYPGEOMDIST not implemented");
    }

    public void testINTERCEPT() throws Exception {
        INTERCEPT i = new INTERCEPT();
        fail("INTERCEPT not implemented");
    }

    public void testKURT() throws Exception {
        KURT k = new KURT();
        assertEquals(eval(k, 11, 2, 33, 44, 44), -2.47630587213087);
        assertEquals(eval(k, 3, 4, 5, 2, 3, 4, 5, 6, 4, 7), -0.151799637208416);
    }

    public void testLARGE() throws Exception {
        LARGE l = new LARGE();
        fail("LARGE not implemented");
    }

    public void testLINEST() throws Exception {
        LINEST l = new LINEST();
        fail("LINEST not implemented");
    }

    public void testLOGEST() throws Exception {
        LOGEST l = new LOGEST();
        fail("LOGEST not implemented");
    }

    public void testMAX() throws Exception {
        MAX m = new MAX();
        assertEquals(eval(m, 10, 7, 9, 27, 2), 27.);
    }

    public void testMAXA() throws Exception {
        MAXA m = new MAXA();
        assertEquals(eval(m, 10, 7, 9, 27, 2), 27.);
    }

    public void testMEDIAN() throws Exception {
        MEDIAN m = new MEDIAN();
        fail("MEDIAN not implemented");
    }

    public void testLOGINV() throws Exception {
        LOGINV l = new LOGINV();
        fail("LOGINV not implemented");
    }

    public void testLOGNORMDIST() throws Exception {
        LOGNORMDIST l = new LOGNORMDIST();
        fail("LOGNORMDIST not implemented");
    }

    public void testMIN() throws Exception {
        MIN m = new MIN();
        assertEquals(eval(m, 10, 7, 9, 27, 2), 2.);
    }

    public void testMINA() throws Exception {
        MINA m = new MINA();
        assertEquals(eval(m, 10, 7, 9, 27, 2), 2.);
    }

    public void testMODE() throws Exception {
        MODE m = new MODE();
        fail("MODE not implemented");
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
        assertEquals(eval(n, 1.333333), 0.908788725604095);
        assertEquals(eval(n, 1), 0.841344746068543);
    }

    public void testNORMSINV() throws Exception {
        NORMSINV n = new NORMSINV();
        fail("NORMSINV not implemented");
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
        assertEquals(eval(p, 4, 3), 24.);
        assertEquals(eval(p, 100, 3), 970200.);
    }

    public void testPOISSON() throws Exception {
        POISSON p = new POISSON();
        fail("POISSON not implemented");
    }

    public void testPROB() throws Exception {
        PROB p = new PROB();
        fail("PROB not implemented");
    }

    public void testQUARTILE() throws Exception {
        QUARTILE q = new QUARTILE();
        fail("QUARTILE not implemented");
    }

    public void testRANK() throws Exception {
        RANK r = new RANK();
        fail("RANK not implemented");
    }

    public void testRSQ() throws Exception {
        RSQ r = new RSQ();
        fail("RSQ not implemented");
    }

    public void testSKEW() throws Exception {
        SKEW s = new SKEW();
        fail("SKEW not implemented");
    }

    public void testSLOPE() throws Exception {
        SLOPE s = new SLOPE();
        fail("SLOPE not implemented");
    }

    public void testSMALL() throws Exception {
        SMALL s = new SMALL();
        fail("SMALL not implemented");
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
        assertResult(
                "stdeva({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                27.4639157198405);
    }

    public void testSTDEVP() throws Exception {
        assertResult(
                "stdevp({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                26.0545581424796);
    }

    public void testSTDEVPA() throws Exception {
        assertResult(
                "stdevpa({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                26.0545581424796);
    }

    public void testSTEYX() throws Exception {
        STEYX s = new STEYX();
        fail("STEYX not implemented");
    }

    public void testTDIST() throws Exception {
        TDIST t = new TDIST();
        fail("TDIST not implemented");
    }

    public void testTINV() throws Exception {
        TINV t = new TINV();
        fail("TINV not implemented");
    }

    public void testTREND() throws Exception {
        TREND t = new TREND();
        fail("TREND not implemented");
    }

    public void testTRIMMEAN() throws Exception {
        TRIMMEAN t = new TRIMMEAN();
        fail("TRIMMEAN not implemented");
    }

    public void testTTEST() throws Exception {
        TTEST t = new TTEST();
        fail("TTEST not implemented");
    }

    public void testVAR() throws Exception {
        assertResult(
                "var({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                754.26666666665);
    }

    public void testVARA() throws Exception {
        assertResult(
                "vara({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                754.26666666665);
    }

    public void testVARP() throws Exception {
        assertResult(
                "varp({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                678.84);
    }

    public void testVARPA() throws Exception {
        assertResult(
                "varpa({1345,1301,1368,1322,1310,1370,1318,1350,1303,1299})",
                678.84);
    }

    public void testWEIBULL() throws Exception {
        WEIBULL w = new WEIBULL();
        fail("WEIBULL not implemented");
    }

    public void testZTEST() throws Exception {
        ZTEST z = new ZTEST();
        fail("ZTEST not implemented");
    }
}
