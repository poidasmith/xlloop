package org.boris.expr.function.excel;

import org.boris.expr.ExprException;
import org.boris.expr.function.DoubleInOutFunction;

public class SIGN extends DoubleInOutFunction
{
    protected double evaluate(double value) throws ExprException {
        if (value < 0)
            return -1;
        else if (value > 0)
            return 1;
        else
            return 0;
    }
}
