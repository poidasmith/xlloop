package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.ExprNumber;
import org.boris.expr.function.AbstractFunction;

public class NOT extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        Expr a = evalArg(args[0]);
        if (a instanceof ExprNumber) {
            return bool(!((ExprNumber) a).booleanValue());
        }
        return ExprError.VALUE;
    }
}
