package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.ExprString;
import org.boris.expr.function.AbstractFunction;

public class UPPER extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        Expr a = evalArg(args[0]);
        if (a instanceof ExprArray)
            return ExprError.VALUE;
        if (a instanceof ExprString)
            return new ExprString(((ExprString) a).str.toUpperCase());
        return new ExprString(a.toString().toUpperCase());
    }
}
