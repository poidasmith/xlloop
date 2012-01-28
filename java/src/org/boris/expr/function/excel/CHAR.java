package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.ExprString;
import org.boris.expr.function.AbstractFunction;

public class CHAR extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        int c = asInteger(args[0], true);
        if (c < 1 || c > 255)
            return ExprError.VALUE;
        else
            return new ExprString("" + (char) c);
    }
}
