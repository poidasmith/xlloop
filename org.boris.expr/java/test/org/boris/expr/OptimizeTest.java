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

public class OptimizeTest extends TH
{
    public void testOptimize1() throws Exception {
        Expr e,eo;
        e = parse("34+56/100");
        eo = e.optimize();
        assertTrue(e instanceof ExprEvaluatable);
        assertTrue(eo instanceof ExprDouble);
        e = parse("-1^2");
        eo = e.optimize();
        assertTrue(eo instanceof ExprDouble);
    }
}
