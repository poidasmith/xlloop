package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;

public class INDIRECT extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertMinArgCount(args, 1);
        assertMaxArgCount(args, 2);
        Expr ref = evalArg(args[0]);

        return null;
    }
}
