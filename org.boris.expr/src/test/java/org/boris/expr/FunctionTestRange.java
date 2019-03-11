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

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;

public class FunctionTestRange extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);

        ExprArray arr = new ExprArray(4, 4);
        for (int i = 0; i < arr.rows(); i++) {
            for (int j = 0; j < arr.columns(); j++) {
                arr.set(i, j, new ExprDouble((i + 1) * (j + 1)));
            }
        }
        return arr;
    }
}
