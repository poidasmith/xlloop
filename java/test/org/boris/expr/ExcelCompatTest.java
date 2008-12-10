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

import junit.framework.TestCase;

public class ExcelCompatTest extends TestCase
{
    public void test1() throws Exception {
        TH.assertResult("-1.3E1/3", new ExprDouble(-1.3E1 / 3));
        TH.assertResult("1.3E-4/3", new ExprDouble(1.3E-4 / 3));
        TH.assertResult("1300000000000000/3", new ExprDouble(
                1300000000000000. / 3));
        TH.assertResult("-10E-1/3.1E2*4E3/3E4", new ExprDouble(
                -10E-1 / 3.1E2 * 4E3 / 3E4));
    }

    public void testReferences() throws Exception {
        TH.assertResult("'Quotes Needed Here &#$@'!A1", new ExprVariable(null,
                "'Quotes Needed Here &#$@'!A1"));
    }

    public void testIf() throws Exception {
        TH.assertResult("IF(3>=1,\"*\",IF(4<>1,\"first\",\"second\"))",
                new ExprString("*"));
        TH.assertResult("IF((A1+A2)<=3,\"yes\",\"no\")", new ExprString("yes"));
        TH.assertResult("IF(A1<A2,B1,B2)", null);
        TH.assertResult("IF(1=1,10)", new ExprInteger(10));
        TH.assertResult("IF(A1=B1,AVERAGE(A1:B1),AVERAGE(A2:B2))",
                ExprError.DIV0);
        TH.assertResult("IF(1=1,0,1)", new ExprInteger(0));
        TH.assertResult("IF(TRUE,\"Y\",\"N\")", new ExprString("Y"));
    }

    public void testSumIf() throws Exception {
        TH.assertResult("SUMIF(A1:A5,\">4000\",B1:B5)", null);
    }
}
