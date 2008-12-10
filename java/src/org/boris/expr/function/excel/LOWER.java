package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprException;
import org.boris.expr.ExprString;
import org.boris.expr.ExprType;
import org.boris.expr.function.AbstractFunction;

public class LOWER extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        assertArgType(args[0], ExprType.String);

        return new ExprString(((ExprString) args[0]).str.toLowerCase());
    }
}
