package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;
import org.boris.expr.util.Maths;

public class ROUNDDOWN extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 2);
        double num = asDouble(args[0], true);
        int dps = asInteger(args[1], true);
        return new ExprDouble(Maths.roundDown(num, dps));
    }
}
