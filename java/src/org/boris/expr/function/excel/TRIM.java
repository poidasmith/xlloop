package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.ExprString;
import org.boris.expr.function.AbstractFunction;

public class TRIM extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        Expr a = evalArg(args[0]);
        if (a instanceof ExprArray)
            return ExprError.VALUE;
        String str = null;
        if (a instanceof ExprString)
            str = ((ExprString) a).str;
        else
            str = a.toString();
        return new ExprString(str.trim());
    }
}
