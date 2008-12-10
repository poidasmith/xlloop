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
import org.boris.variant.util.Reflect;

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
        return evaluateExpr(lhs);
    }

    protected double evaluateRHS() throws ExprException {
        return evaluateExpr(rhs);
    }

    protected double evaluateExpr(Expr expr) throws ExprException {
        Expr r = expr;
        if (r == null)
            return 0;
        if (r instanceof ExprMissing)
            return 0;
        if (r instanceof ExprEvaluatable)
            r = ((ExprEvaluatable) r).evaluate();
        if (r instanceof ExprMissing)
            return 0;
        if (r == null)
            return 0;
        ExprTypes.assertType(r, ExprType.Integer, ExprType.Double);
        return ((ExprNumber) r).doubleValue();
    }

    public void validate() throws ExprException {
        if (lhs == null)
            throw new ExprException("LHS of operator missing");
        lhs.validate();
        if (rhs == null)
            throw new ExprException("RHS of operator missing");
        rhs.validate();
    }

    public Variant encode() {
        VTMap m = new VTMap();
        m.add("type", type.toString());
        if (lhs != null)
            m.add("lhs", lhs.encode());
        if (rhs != null)
            m.add("rhs", rhs.encode());
        return m;
    }

    public int hashCode() {
        int hc = type.ordinal();
        if (lhs != null)
            hc ^= lhs.hashCode();
        if (rhs != null)
            hc ^= rhs.hashCode();
        return hc;
    }

    public boolean equals(Object obj) {
        if (!obj.getClass().equals(getClass()))
            return false;

        AbstractBinaryOperator b = (AbstractBinaryOperator) obj;
        return Reflect.equals(b.lhs, lhs) && Reflect.equals(b.rhs, rhs);
    }
}
