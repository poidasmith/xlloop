package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;

public class POWER extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 2);
        double num = asDouble(args[0], true);
        double pow = asDouble(args[1], true);
        return new ExprDouble(Math.pow(num, pow));
    }
}
