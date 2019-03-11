package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprNumber;
import org.boris.expr.function.AbstractFunction;

public class MOD extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 2);

        Expr n = args[0];
        if (n instanceof ExprEvaluatable) {
            n = ((ExprEvaluatable) n).evaluate();
        }
        if (n instanceof ExprArray) {
            ExprArray a = (ExprArray) n;
            if (a.rows() > 1) {
                return ExprError.VALUE;
            }

            n = a.get(0, 0);
        }
        if (!(n instanceof ExprNumber)) {
            return ExprError.VALUE;
        }

        double num = ((ExprNumber) n).doubleValue();

        Expr d = args[1];
        if (d instanceof ExprEvaluatable) {
            d = ((ExprEvaluatable) d).evaluate();
        }
        if (!(d instanceof ExprNumber)) {
            return ExprError.VALUE;
        }

        double div = ((ExprNumber) d).doubleValue();

        // Need to match sign with implementation
        double mod = num % div;
        if ((mod > 0 && div < 0) || (mod < 0 && div > 0))
            mod *= -1;

        return new ExprDouble(mod);
    }
}
