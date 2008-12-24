package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprException;
import org.boris.expr.ExprInteger;
import org.boris.expr.function.AbstractFunction;

public class ISNUMBER extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        Expr e = evalArg(args[0]);
        return bool(e instanceof ExprInteger || e instanceof ExprDouble);
    }
}
