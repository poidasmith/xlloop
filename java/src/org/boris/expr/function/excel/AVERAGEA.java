package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprMissing;
import org.boris.expr.ExprNumber;
import org.boris.expr.ExprString;
import org.boris.expr.function.AbstractFunction;

public class AVERAGEA extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertMinArgCount(args, 1);

        double[] values = { 0, 0 };
        for (Expr a : args)
            eval(a, values);

        if (values[1] == 0) {
            return ExprError.DIV0;
        }

        return new ExprDouble(values[0] / values[1]);
    }

    public static void eval(Expr a, double[] values) throws ExprException {
        if (a instanceof ExprEvaluatable)
            a = ((ExprEvaluatable) a).evaluate();

        if (a == null)
            return;

        if (a instanceof ExprMissing)
            return;

        if (a instanceof ExprString) {
            values[1] += 1;
        }

        if (a instanceof ExprNumber) {
            double d = ((ExprNumber) a).doubleValue();
            values[0] += d;
            values[1] += 1;
            return;
        }

        if (a instanceof ExprArray) {
            ExprArray arr = (ExprArray) a;
            int rows = arr.rows();
            int cols = arr.columns();
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    eval(arr.get(i, j), values);
                }
            }

            return;
        }

        throw new ExprException("Unexpected argument for AVERAGEA: " + a);
    }

}
