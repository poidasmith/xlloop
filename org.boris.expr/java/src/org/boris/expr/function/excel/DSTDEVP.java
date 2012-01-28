package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprException;
import org.boris.expr.function.SimpleDatabaseFunction;

public class DSTDEVP extends SimpleDatabaseFunction
{
    protected Expr evaluateMatches(Expr[] matches) throws ExprException {
        return STDEVP.stdevp(matches);
    }
}
