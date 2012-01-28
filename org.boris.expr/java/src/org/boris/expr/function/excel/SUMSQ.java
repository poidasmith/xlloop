package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprNumber;
import org.boris.expr.function.AbstractFunction;

public class SUMSQ extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        double res = 0;
        for (Expr arg : args)
            res += sumsq(arg);
        return new ExprDouble(res);
    }

    private double sumsq(Expr arg) throws ExprException {
        if (arg instanceof ExprEvaluatable) {
            arg = ((ExprEvaluatable) arg).evaluate();
        }

        if (arg instanceof ExprNumber) {
            return Math.pow(((ExprNumber) arg).doubleValue(), 2);
        } else if (arg instanceof ExprArray) {
            ExprArray a = (ExprArray) arg;
            int rows = a.rows();
            int cols = a.columns();
            double res = 0;
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    res += sumsq(a.get(i, j));
                }
            }
            return res;
        }

        return 0;
    }
}
