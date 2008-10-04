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

public class ExprStringConcat extends ExprEvaluatable implements
        IBinaryOperator
{
    private Expr lhs;
    private Expr rhs;

    public ExprStringConcat(Expr lhs, Expr rhs) {
        super(ExprType.StringConcat);
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
        if (lhs.type.equals(ExprType.String) &&
                rhs.type.equals(ExprType.String)) {
            return new ExprString(((ExprString) lhs).str +
                    ((ExprString) rhs).str);
        }

        throw new ExprException("Unexpected arguments for string concatenation");
    }

    public String toString() {
        return lhs + "&" + rhs;
    }

    public Variant encode() {
        VTMap m = new VTMap();
        m.add("type", type.toString());
        m.add("lhs", lhs.encode());
        m.add("rhs", rhs.encode());
        return m;
    }
}
