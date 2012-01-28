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

public class ExprFunctionTest extends TestCase
{
    public void testArgs() throws Exception {
        BasicEvaluationCallback b = new BasicEvaluationCallback();
        ExprFunction f = (ExprFunction) b
                .parse("test(1,\"hello\",3.54,1+2,5*5)");
        assertEquals(new ExprInteger(1), f.getArg(0));
        assertEquals(new ExprString("hello"), f.getArg(1));
        assertEquals(new ExprDouble(3.54), f.getArg(2));
        assertEquals(b.parse("1+2"), f.getArg(3));
        assertEquals(new ExprAddition(new ExprInteger(1), new ExprInteger(2)),
                f.getArg(3));
        assertEquals(new ExprMultiplication(new ExprInteger(5),
                new ExprInteger(5)), f.getArg(4));
    }

    public void testMissing() throws Exception {
        BasicEvaluationCallback b = new BasicEvaluationCallback();
        ExprFunction f = (ExprFunction) b.parse("test(123,,34)");
        assertEquals(new ExprInteger(123), f.getArg(0));
        assertEquals(ExprMissing.MISSING, f.getArg(1));
        assertEquals(new ExprInteger(34), f.getArg(2));
    }
}
