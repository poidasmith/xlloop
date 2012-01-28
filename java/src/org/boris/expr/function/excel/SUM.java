package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprNumber;
import org.boris.expr.function.AbstractFunction;

public class SUM extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertMinArgCount(args, 1);
        return sum(args);
    }

    public static Expr sum(Expr[] args) throws ExprException {
        double res = 0;
        for (Expr arg : args)
            res += sum(arg);
        return new ExprDouble(res);
    }

    public static double sum(Expr arg) throws ExprException {
        if (arg instanceof ExprEvaluatable) {
            arg = ((ExprEvaluatable) arg).evaluate();
        }

        if (arg instanceof ExprNumber) {
            return ((ExprNumber) arg).doubleValue();
        } else if (arg instanceof ExprArray) {
            ExprArray a = (ExprArray) arg;
            int rows = a.rows();
            int cols = a.columns();
            double res = 0;
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    res += sum(a.get(i, j));
                }
            }
            return res;
        }
        return 0;
    }
}
