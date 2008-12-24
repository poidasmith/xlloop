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

import org.boris.expr.util.Criteria;
import org.boris.expr.util.Database;

public class ExcelDatabaseFunctionsTest extends TH
{
    public void testDatabase() throws Exception {
        Database db = Database.valueOf(loadArray("db1-d.txt"));
        assertEquals(db.get(2, "Yield"), 9);
        Criteria c = Criteria.valueOf(loadArray("db1-c.txt"));
        assertEquals(c.matches(db, 3), true);
    }

    public void testDAVERAGE() throws Exception {
        assertResult(c(), "DAVERAGE(A4:E10,\"Yield\",A1:B2)", 12.);
        assertResult(c(), "DAVERAGE(A4:E10,3,A4:E10)", 13.);
    }

    public void testDCOUNT() throws Exception {
        assertResult(c(), "DCOUNT(A4:E10,\"Age\",A1:F2)", 1.);
    }

    public void testDCOUNTA() throws Exception {
        assertResult(c(), "DCOUNTA(A4:E10,\"Age\",A1:F2)", 1.);
    }

    public void testDGET() throws Exception {
        assertResult(c(), "DGET(A4:E10,\"Yield\",A1:A3)", ExprError.NUM);
    }

    public void testDMAX() throws Exception {
        assertResult(c(), "DMAX(A4:E10,\"Profit\",A1:A3)", 105.);
    }

    public void testDMIN() throws Exception {
        assertResult(c(), "DMIN(A4:E10,\"Profit\",A1:B2)", 75.);
    }

    public void testDPRODUCT() throws Exception {
        assertResult(c(), "DPRODUCT(A4:E10,\"Yield\",A1:B2)", 140.);
    }

    public void testDSTDEV() throws Exception {
        assertResult(c(), "DSTDEV(A4:E10,\"Yield\",A1:A3)", 2.96647939483826);
    }

    public void testDSTDEVP() throws Exception {
        assertResult(c(), "DSTDEVP(A4:E10,\"Yield\",A1:A3)", 2.65329983228432);
    }

    public void testDSUM() throws Exception {
        assertResult(c(), "DSUM(A4:E10,\"Profit\",A1:A2)", 225.);
        assertResult(c(), "DSUM(A4:E10,\"Profit\",A1:F2)", 75.);
    }

    public void testDVAR() throws Exception {
        assertResult(c(), "DVAR(A4:E10,\"Yield\",A1:A3)", 8.8);
    }

    public void testDVARP() throws Exception {
        assertResult(c(), "DVARP(A4:E10,\"Yield\",A1:A3)", 7.04);
    }

    private static BasicEvaluationCallback c() throws Exception {
        BasicEvaluationCallback c = new BasicEvaluationCallback();
        c.set(loadArray("db1.txt"));
        return c;
    }
}
