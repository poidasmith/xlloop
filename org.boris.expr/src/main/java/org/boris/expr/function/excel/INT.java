package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprBoolean;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprInteger;
import org.boris.expr.ExprNumber;
import org.boris.expr.function.AbstractFunction;

public class INT extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 1);
        Expr a = args[0];
        if (a instanceof ExprEvaluatable) {
            a = ((ExprEvaluatable) a).evaluate();
        }
        if (a instanceof ExprInteger) {
            return a;
        } else if (a instanceof ExprBoolean) {
            return new ExprInteger(((ExprNumber) a).intValue());
        } else if (a instanceof ExprDouble) {
            return new ExprInteger((int) Math.floor(((ExprDouble) a)
                    .doubleValue()));
        }

        return ExprError.VALUE;
    }
}
