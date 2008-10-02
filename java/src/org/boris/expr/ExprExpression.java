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

public class ExprExpression extends ExprEvaluatable
{
    private Expr child;

    public ExprExpression(Expr child) {
        super(ExprType.Expression);
        this.child = child;
    }

    public Expr evaluate() throws ExprException {
        return child;
    }

    public String toString() {
        return "(" + child + ")";
    }
}
