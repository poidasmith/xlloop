package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.ExprNumber;
import org.boris.expr.function.AbstractFunction;
import org.boris.expr.util.ExcelDate;

public class TIME extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 3);
        Expr eH = evalArg(args[0]);
        if (!isNumber(eH))
            return ExprError.VALUE;
        double h = ((ExprNumber) eH).doubleValue();
        Expr eM = evalArg(args[1]);
        if (!isNumber(eM))
            return ExprError.VALUE;
        double m = ((ExprNumber) eM).doubleValue();
        Expr eS = evalArg(args[1]);
        if (!isNumber(eS))
            return ExprError.VALUE;
        double s = ((ExprNumber) eS).doubleValue();
        return time(h, m, s);
    }

    public static Expr time(double h, double m, double s) {
        h %= 24;

        double t = (h + m + s) / ExcelDate.MS_IN_DAY;
        if (t < 0 || t >= 1)
            return ExprError.NUM;
        return new ExprDouble(t);
    }
}
