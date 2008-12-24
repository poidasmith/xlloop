package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.ExprString;
import org.boris.expr.function.AbstractFunction;

public class MID extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 3);
        String str = asString(args[0], false);
        int start = asInteger(args[1], true);
        if (start < 1)
            return ExprError.VALUE;
        int len = asInteger(args[2], true);
        if (len < 0)
            return ExprError.VALUE;
        int stlen = str.length();
        if (start > stlen)
            start = stlen + 1;
        if (start + len > stlen)
            len = stlen - start + 1;
        if (len < 0)
            len = 0;
        return new ExprString(str.substring(start - 1, start + len - 1));
    }
}
