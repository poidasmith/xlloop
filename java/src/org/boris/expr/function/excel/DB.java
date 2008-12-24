package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;
import org.boris.expr.util.Financials;

public class DB extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertMinArgCount(args, 4);
        assertMaxArgCount(args, 5);

        double cost = asDouble(args[0], true);
        if (cost < 0)
            return ExprError.NUM;
        double salvage = asDouble(args[1], true);
        if (salvage < 0)
            return ExprError.NUM;
        int life = asInteger(args[2], true);
        if (life < 0)
            return ExprError.NUM;
        int period = asInteger(args[3], true);
        if (period < 0)
            return ExprError.NUM;
        int month = 12;
        if (args.length == 5)
            month = asInteger(args[4], true);
        if (month < 0)
            return ExprError.NUM;

        if (cost == 0)
            return ExprDouble.ZERO;

        return new ExprDouble(Financials.db(cost, salvage, life, period, month));
    }
}
