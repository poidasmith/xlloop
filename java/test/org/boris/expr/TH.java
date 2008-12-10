/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr;

import java.io.IOException;

import junit.framework.TestCase;

import org.boris.expr.function.DoubleInOutFunction;

public class TH extends TestCase
{
    public static void assertException(String expression) throws Exception {
        try {
            BasicEvaluationCallback b = new BasicEvaluationCallback();
            Expr e = b.parse(expression);
            if (e instanceof ExprEvaluatable)
                ((ExprEvaluatable) e).evaluate();
            TestCase.fail("Expected an exception");
        } catch (Exception ex) {
        }
    }

    public static void assertResult(String expression, Object result)
            throws Exception {
        assertResult(expression, convertObject(result));
    }

    public static Expr parse(String expression) throws IOException,
            ExprException {
        return new BasicEvaluationCallback().parse(expression);
    }

    public static void assertResult(String expression, Expr result)
            throws Exception {
        BasicEvaluationCallback c = new BasicEvaluationCallback();
        Expr e = c.parse(expression);
        if (!(result instanceof ExprEvaluatable)) {
            if (e instanceof ExprEvaluatable) {
                e = ((ExprEvaluatable) e).evaluate();
            }
            if (e instanceof ExprEvaluatable) {
                e = ((ExprEvaluatable) e).evaluate();
            }
        }
        TestCase.assertEquals(e, result);
    }

    public static void testDoubleInOutFunction(DoubleInOutFunction f)
            throws Exception {
    }

    public static Object eval(IExprFunction function, Object... args)
            throws ExprException {
        return convertExpr(function.evaluate(convertArgs(args)));
    }

    public static Expr[] convertArgs(Object[] args) {
        Expr[] a = new Expr[args.length];
        for (int i = 0; i < args.length; i++) {
            a[i] = convertObject(args[i]);
        }
        return a;
    }

    public static Object convertExpr(Expr e) throws ExprException {
        if (e == null)
            return null;

        if (e instanceof ExprEvaluatable)
            e = ((ExprEvaluatable) e).evaluate();

        if (e instanceof ExprString)
            return ((ExprString) e).str;

        if (e instanceof ExprDouble)
            return ((ExprDouble) e).doubleValue();

        if (e instanceof ExprInteger)
            return ((ExprInteger) e).intValue();

        if (e instanceof ExprBoolean)
            return ((ExprBoolean) e).booleanValue();

        return e;
    }

    public static Expr convertObject(Object o) {
        if (o == null)
            return null;

        if (o instanceof Double)
            return new ExprDouble(((Double) o).doubleValue());

        if (o instanceof Integer)
            return new ExprInteger(((Integer) o).intValue());

        if (o instanceof Boolean)
            return new ExprBoolean(((Boolean) o).booleanValue());

        if (o instanceof String)
            return new ExprString((String) o);

        if (o instanceof Expr)
            return (Expr) o;

        return null;
    }

    public static void assertException(IExprFunction function, Object... args) {
        try {
            function.evaluate(convertArgs(args));
            TestCase.fail("Expected exception");
        } catch (ExprException e) {
        }
    }

    public static void assertException(IExprFunction function, Expr... args) {
        try {
            function.evaluate(args);
            TestCase.fail("Expected exception");
        } catch (ExprException e) {
        }
    }

    static public boolean assertEquals(Double val, Double val2) {
        if (Math.abs(val - val2) > 0.00000000001) {
            TestCase.failNotSame(null, val, val2);
            return false;
        }
        return true;
    }

    public static boolean assertEquals(Object o, Double val) {
        return assertEquals((Double) o, val);
    }

    public static ExprArray toArray(Object... args) {
        Expr[] a = convertArgs(args);
        ExprArray arr = new ExprArray(1, a.length);
        for (int i = 0; i < a.length; i++) {
            arr.set(0, i, a[i]);
        }
        return arr;
    }
}
