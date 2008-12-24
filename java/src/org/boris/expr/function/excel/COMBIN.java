package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;
import org.boris.expr.util.Statistics;

public class COMBIN extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 2);
        int num = asInteger(args[0], true);
        int cho = asInteger(args[1], true);
        if (num < 0 || cho < 0 || num < cho)
            return ExprError.NUM;

        return new ExprDouble(Statistics.combin(num, cho));
    }
}
