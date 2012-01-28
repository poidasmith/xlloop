package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;
import org.boris.expr.util.ValueParser;

public class VALUE extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        Expr e = evalArg(args[0]);
        if (e instanceof ExprError)
            return e;
        if (e instanceof ExprArray)
            return ExprError.VALUE;

        String s = asString(e, false);
        Double d = ValueParser.parse(s);

        if (d != null)
            return new ExprDouble(d.doubleValue());
        else
            return ExprError.VALUE;
    }
}
