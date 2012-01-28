package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;

public class LOOKUP extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertMinArgCount(args, 2);
        assertMaxArgCount(args, 3);
        if (args.length == 2) {
            return arrayLookup(args);
        } else {
            return vectorLookup(args);
        }
    }

    public static Expr vectorLookup(Expr[] args) throws ExprException {
        Expr ev = evalArg(args[0]);
        Expr el = evalArg(args[1]);
        Expr er = evalArg(args[2]);
        return null;
    }

    public static Expr arrayLookup(Expr[] args) {
        return null;
    }
}
