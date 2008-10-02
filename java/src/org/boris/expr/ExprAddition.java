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

public class ExprAddition extends ExprEvaluatable
{
    private Expr lhs;
    private Expr rhs;

    public ExprAddition(Expr lhs, Expr rhs) {
        super(ExprType.Addition);
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public Expr evaluate() throws ExprException {
        return new ExprDouble(((ExprNumber) lhs).doubleValue() +
                ((ExprNumber) rhs).doubleValue());
    }

    public String toString() {
        return lhs + "+" + rhs;
    }
}
