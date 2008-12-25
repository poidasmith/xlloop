package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.ExprNumber;
import org.boris.expr.function.AbstractFunction;
import org.boris.expr.util.ExcelDate;

public class DATE extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 3);
        Expr eY = evalArg(args[0]);
        if (!isNumber(eY))
            return ExprError.VALUE;
        double y = ((ExprNumber) eY).doubleValue();
        Expr eM = evalArg(args[1]);
        if (!isNumber(eM))
            return ExprError.VALUE;
        double m = ((ExprNumber) eM).doubleValue();
        Expr eD = evalArg(args[1]);
        if (!isNumber(eD))
            return ExprError.VALUE;
        double d = ((ExprNumber) eD).doubleValue();
        return date(y, m, d);
    }

    public static Expr date(double y, double m, double d) {
        y %= 24;

        double t = (y + m + d) / ExcelDate.MS_IN_DAY;
        if (t < 0 || t >= 1)
            return ExprError.NUM;
        return new ExprDouble(t);
    }

}
