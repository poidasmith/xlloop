package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprException;
import org.boris.expr.ExprNumber;
import org.boris.expr.ExprString;
import org.boris.expr.function.AbstractFunction;

public class CONCATENATE extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertMinArgCount(args, 1);
        StringBuilder sb = new StringBuilder();
        for (Expr a : args) {
            a = evalArg(a);
            if (a instanceof ExprString) {
                sb.append(((ExprString) a).str);
            } else if (a instanceof ExprNumber) {
                sb.append(a.toString());
            }
        }
        return new ExprString(sb.toString());
    }
}
