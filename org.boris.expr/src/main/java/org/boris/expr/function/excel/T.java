package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprException;
import org.boris.expr.ExprString;
import org.boris.expr.function.AbstractFunction;

public class T extends AbstractFunction
{
    private static final ExprString EMPTY = new ExprString("");

    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        Expr a = evalArg(args[0]);

        if (a instanceof ExprString) {
            return a;
        } else {
            return EMPTY;
        }
    }
}
