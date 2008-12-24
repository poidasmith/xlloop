package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;

public class TRUNC extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertMinArgCount(args, 1);
        assertMaxArgCount(args, 2);
        double num = asDouble(args[0], true);
        int dig = 1;
        if (args.length == 2)
            dig = asInteger(args[1], true);
        if (dig == 1) {
            return new ExprDouble((int) num);
        } else {
            int div = (int) Math.pow(10, dig);
            int v = (int) (num * div);
            double d = (double) v / div;
            return new ExprDouble(d);
        }
    }
}
