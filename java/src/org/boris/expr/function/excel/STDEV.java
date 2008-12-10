package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprInteger;
import org.boris.expr.ExprMissing;
import org.boris.expr.ExprNumber;
import org.boris.expr.ExprString;
import org.boris.expr.function.AbstractFunction;

public class STDEV extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        return stdev(args, false);
    }

    protected Expr variance(Expr[] args, boolean allPopulation)
            throws ExprException {
        assertMinArgCount(args, 1);

        double[] values = { 0, 0 };

        for (Expr a : args)
            AVERAGE.eval(a, values, true);

        if (values[1] == 0) {
            return ExprError.NUM;
        }

        double average = values[0] / values[1];

        values[0] = values[1] = 0;

        for (Expr a : args)
            eval(a, average, values, true);

        return new ExprDouble(values[0] / (values[1] - (allPopulation ? 0 : 1)));
    }

    protected Expr stdev(Expr[] args, boolean allPopulation)
            throws ExprException {
        Expr res = variance(args, allPopulation);
        if (res instanceof ExprDouble) {
            res = new ExprDouble(Math.sqrt(((ExprDouble) res).doubleValue()));
        }
        return res;
    }

    protected void eval(Expr a, double average, double[] values, boolean strict)
            throws ExprException {
        if (a instanceof ExprEvaluatable)
            a = ((ExprEvaluatable) a).evaluate();

        if (a == null)
            return;

        if (a instanceof ExprMissing)
            return;

        if (a instanceof ExprString) {
            if (strict)
                throw new ExprException("Unexpected argument for AVERAGE: " + a);
            else
                return;
        }

        if (a instanceof ExprDouble || a instanceof ExprInteger) {
            double d = ((ExprNumber) a).doubleValue();
            values[0] += Math.pow(average - d, 2);
            values[1] += 1;
            return;
        }

        if (a instanceof ExprArray) {
            ExprArray arr = (ExprArray) a;
            int rows = arr.rows();
            int cols = arr.columns();
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    eval(arr.get(i, j), average, values, false);
                }
            }

            return;
        }

        throw new ExprException("Unexpected argument for STDEV: " + a);
    }
}
