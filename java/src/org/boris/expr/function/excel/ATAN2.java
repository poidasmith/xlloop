package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;

public class ATAN2 extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 2);
        double x = asDouble(args[0], true);
        double y = asDouble(args[1], true);
        return new ExprDouble(Math.atan2(y, x));
    }
}
