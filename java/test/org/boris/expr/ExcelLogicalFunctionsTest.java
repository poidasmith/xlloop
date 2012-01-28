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

import org.boris.expr.function.excel.AND;
import org.boris.expr.function.excel.FALSE;
import org.boris.expr.function.excel.IF;
import org.boris.expr.function.excel.NOT;

public class ExcelLogicalFunctionsTest extends TH
{
    public void testAND() throws Exception {
        AND a = new AND();
        assertEquals(TH.eval(a, 3, true, 3.4), true);
        assertEquals(TH.eval(a, 3, false, 3.4), false);
        TH.assertException(a);
        TH.assertException(a, "asdf");
    }

    public void testFALSE() throws Exception {
        FALSE f = new FALSE();
        assertException("false(1)");
        assertEquals(eval(f), false);
    }

    public void testIF() throws Exception {
        IF i = new IF();
        assertResult("if(10<100,4,5)", 4);
        assertEquals(eval(i, parse("10.1>=11"), "right", "wrong"), "wrong");
        assertResult("IF(3>=1,\"*\",IF(4<>1,\"first\",\"second\"))", "*");
        assertResult("IF((A1+A2)<=3,\"yes\",\"no\")", "yes");
        assertResult("IF(A1<A2,B1,B2)", null);
        assertResult("IF(1=1,10)", 10);
        assertResult("IF(A1=B1,AVERAGE(A1:B1),AVERAGE(A2:B2))", ExprError.DIV0);
        assertResult("IF(1=1,0,1)", 0);
        assertResult("IF(TRUE,\"Y\",\"N\")", "Y");
    }

    public void testNOT() throws Exception {
        NOT n = new NOT();
        assertEquals(eval(n, true), false);
        assertEquals(eval(n, 1), false);
        assertEquals(eval(n, 0), true);
        assertEquals(eval(n, -10.3), false);
        assertException("not(123,23)");
    }

    public void testOR() throws Exception {
        assertResult("or(true)", true);
        assertResult("or(1+1=1,2+2=5)", false);
        assertResult("or(true,false,true)", true);
        assertResult("or({false,false,false,true})", true);
    }

    public void testTRUE() throws Exception {
        assertResult("true()", ExprBoolean.TRUE);
        assertException("true(1)");
        assertException("true(234,34,34)");
    }
}
