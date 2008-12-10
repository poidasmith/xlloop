package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprException;

public class STDEVP extends STDEV
{
    public Expr evaluate(Expr[] args) throws ExprException {
        return stdev(args, true);
    }
}
