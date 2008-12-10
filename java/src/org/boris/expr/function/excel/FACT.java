package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;

public class FACT extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        double value = asDouble(args[0], true);
        if (value < 0)
            return ExprError.NUM;
        return new ExprDouble(factorial((int) value));
    }

    private int factorial(int value) {
        if (value <= 1)
            return 1;
        return value * factorial(value - 1);
    }
}
