package org.boris.expr.function.excel;

import org.boris.expr.AbstractBinaryOperator;
import org.boris.expr.Expr;
import org.boris.expr.ExprBoolean;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprEqual;
import org.boris.expr.ExprException;
import org.boris.expr.ExprGreaterThan;
import org.boris.expr.ExprGreaterThanOrEqualTo;
import org.boris.expr.ExprInteger;
import org.boris.expr.ExprLessThan;
import org.boris.expr.ExprLessThanOrEqualTo;
import org.boris.expr.ExprMissing;
import org.boris.expr.ExprNotEqual;
import org.boris.expr.ExprNumber;
import org.boris.expr.ExprString;
import org.boris.expr.function.AbstractFunction;

public class SUMIF extends AbstractFunction
{
    public Expr evaluate(Expr[] args) throws ExprException {
        assertMinArgCount(args, 2);

        /*
        // First argument must be a reference to a range
        if (!(args[0] instanceof ExprVariable)) {
            throw new ExprException(
                    "First argument to SUMIF must be a reference");
        }

        // Sum range (if present) must be a reference to a range
        if (args.length > 2 && !(args[2] instanceof ExprVariable)) {
            throw new ExprException(
                    "Third argument to SUMIF must be a reference");
        }*/

        Expr range = evalArg(args[0]);
        int len = getLength(range);
        Cond cond = Cond.valueOf(evalArg(args[1]));
        Expr sumrange = args.length == 3 ? evalArg(args[2]) : range;

        double sum = 0;
        for (int i = 0; i < len; i++) {
            sum += eval(get(range, i), cond, get(sumrange, i));
        }

        return new ExprDouble(sum);
    }

    protected double eval(Expr item, Cond c, Expr value) throws ExprException {
        if (c.eval(item)) {
            if (value instanceof ExprDouble || value instanceof ExprInteger) {
                return ((ExprNumber) value).doubleValue();
            }
        }
        return 0.;
    }

    protected static class Cond
    {
        protected AbstractBinaryOperator operator;

        public static Cond valueOf(Expr arg) {
            if (arg instanceof ExprString) {
                String s = ((ExprString) arg).str;
                Cond c = new Cond();
                int offset = 0;
                boolean str = false;
                if (s.startsWith(">=")) {
                    c.operator = new ExprGreaterThanOrEqualTo(null, null);
                    offset = 2;
                } else if (s.startsWith("<=")) {
                    c.operator = new ExprLessThanOrEqualTo(null, null);
                    offset = 2;
                } else if (s.startsWith("<>")) {
                    c.operator = new ExprNotEqual(null, null);
                    offset = 2;
                } else if (s.startsWith("=")) {
                    c.operator = new ExprEqual(null, null);
                    offset = 1;
                } else if (s.startsWith("<")) {
                    c.operator = new ExprLessThan(null, null);
                    offset = 1;
                } else if (s.startsWith(">")) {
                    c.operator = new ExprGreaterThan(null, null);
                    offset = 1;
                } else {
                    c.operator = new ExprEqual(null, null);
                    str = true;
                    offset = 0;
                }

                c.operator.setRHS(c.getRHS(s, offset, str));
                return c;
            } else if (arg instanceof ExprDouble || arg instanceof ExprInteger) {
                Cond c = new Cond();
                c.operator = new ExprEqual(null, arg);
                return c;
            }
            return null;
        }

        public Expr getRHS(String text, int offset, boolean str) {
            if (str) {
                return new ExprString(text.substring(offset));
            } else {
                try {
                    return new ExprDouble(Double.parseDouble(text
                            .substring(offset)));
                } catch (NumberFormatException e) {
                    return ExprMissing.MISSING;
                }
            }
        }

        public boolean eval(Expr arg) throws ExprException {
            if (operator == null) {
                return false;
            } else {
                operator.setLHS(arg);
                ExprBoolean v = (ExprBoolean) operator.evaluate();
                return v.value;
            }
        }
    }
}
