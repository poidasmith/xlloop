package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;

public class ISNA extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        Expr e = evalArg(args[0]);
        return bool(ExprError.NA.equals(e));
    }
}
