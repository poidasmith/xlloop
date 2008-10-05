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

import org.boris.variant.VTMap;
import org.boris.variant.Variant;

public class ExprMultiplication extends ExprEvaluatable implements
        IBinaryOperator
{
    private Expr rhs;
    private Expr lhs;

    public ExprMultiplication(Expr lhs, Expr rhs) {
        super(ExprType.Multiplication);
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public Expr getLHS() {
        return lhs;
    }

    public void setLHS(Expr lhs) {
        this.lhs = lhs;
    }

    public Expr getRHS() {
        return rhs;
    }

    public void setRHS(Expr rhs) {
        this.rhs = rhs;
    }

    public Expr evaluate() throws ExprException {
        Expr l = lhs;
        if (l instanceof ExprEvaluatable)
            l = ((ExprEvaluatable) l).evaluate();
        Expr r = rhs;
        if (r instanceof ExprEvaluatable)
            r = ((ExprEvaluatable) r).evaluate();
        return new ExprDouble(((ExprNumber) l).doubleValue() *
                ((ExprNumber) r).doubleValue());
    }

    public String toString() {
        return lhs + "*" + rhs;
    }

    public Variant encode() {
        VTMap m = new VTMap();
        m.add("type", type.toString());
        m.add("lhs", lhs.encode());
        m.add("rhs", rhs.encode());
        return m;
    }
}