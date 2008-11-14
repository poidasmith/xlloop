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

public abstract class AbstractBinaryOperator extends ExprEvaluatable implements
        IBinaryOperator
{
    protected Expr lhs;
    protected Expr rhs;

    public AbstractBinaryOperator(ExprType type, Expr lhs, Expr rhs) {
        super(type);
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

    protected double evaluateLHS() throws ExprException {
        Expr l = lhs;
        if (l instanceof ExprEvaluatable)
            l = ((ExprEvaluatable) l).evaluate();
        ExprTypes.assertType(l, ExprType.Integer, ExprType.Double);
        return ((ExprNumber) l).doubleValue();
    }

    protected double evaluateRHS() throws ExprException {
        Expr r = rhs;
        if (r instanceof ExprEvaluatable)
            r = ((ExprEvaluatable) r).evaluate();
        ExprTypes.assertType(r, ExprType.Integer, ExprType.Double);
        return ((ExprNumber) r).doubleValue();
    }

    public void validate() throws ExprException {
    }

    public Variant encode() {
        VTMap m = new VTMap();
        m.add("type", type.toString());
        m.add("lhs", lhs.encode());
        m.add("rhs", rhs.encode());
        return m;
    }
}
