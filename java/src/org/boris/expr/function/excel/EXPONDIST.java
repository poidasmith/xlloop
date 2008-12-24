package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;
import org.boris.expr.util.Statistics;

public class EXPONDIST extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 3);
        double x = asDouble(args[0], true);
        double l = asDouble(args[1], true);
        boolean c = asBoolean(args[2], true);

        return new ExprDouble(Statistics.exponDist(x, l, c));
    }
}
