package org.boris.expr.function.excel;

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.function.AbstractFunction;
import org.boris.expr.util.Condition;

public class COUNTIF extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 2);
        // TODO assert first argument as reference
        ExprArray array = asArray(args[0], true);
        Condition cond = Condition.valueOf(args[1]);
        if (cond == null)
            return ExprError.VALUE;
        Expr[] a = array.getArgs();
        int count = 0;
        for (int i = 0; i < a.length; i++) {
            if (cond.eval(a[i]))
                count++;
        }
        return new ExprDouble(count);
    }
}
