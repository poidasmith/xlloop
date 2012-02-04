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

import org.boris.expr.engine.Range;
import org.boris.expr.function.DoubleInOutFunction;
import org.boris.expr.util.CSV;
import org.boris.expr.util.ExprArrayBuilder;
import org.boris.expr.util.Exprs;
import org.boris.expr.util.IO;

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
        assertResult(expression, Exprs.convertObject(result));
    }

    public static Expr parse(String expression) throws IOException,
            ExprException {
        return new BasicEvaluationCallback().parse(expression);
    }

    public static void assertResult(String expression, Expr result)
            throws Exception {
        assertResult(expression, result, !(result instanceof ExprEvaluatable));
    }

    public static void assertResult(String expression, Expr result, boolean eval)
            throws Exception {
        BasicEvaluationCallback c = new BasicEvaluationCallback();
        assertResult(c, expression, result, eval);
    }

    public static void assertResult(BasicEvaluationCallback c,
            String expression, Object result) throws Exception {
        assertResult(c, expression, result,
                !(result instanceof ExprEvaluatable));
    }

    public static void assertResult(BasicEvaluationCallback c,
            String expression, Object result, boolean eval) throws Exception {
        Expr e = c.parse(expression);
        if (eval) {
            if (e instanceof ExprEvaluatable) {
                e = ((ExprEvaluatable) e).evaluate();
            }
        }
        TestCase.assertEquals(e, Exprs.convertObject(result));
    }

    public static void testDoubleInOutFunction(DoubleInOutFunction f)
            throws Exception {
    }

    public static Object eval(IExprFunction function, Object... args)
            throws ExprException {
        return Exprs.convertExpr(function.evaluate(Exprs.convertArgs(args)));
    }

    public static void assertException(IExprFunction function, Object... args) {
        try {
            function.evaluate(Exprs.convertArgs(args));
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

    public static void assertEquals(Expr e, Object o) throws ExprException {
        assertEquals(Exprs.convertExpr(e), o);
    }
    
    public static void assertEquals(Expr e, Double o) throws ExprException {
        assertEquals(Exprs.convertExpr(e), o);
    }

    public static void assertEquals(Object o, Double val) {
        if (o instanceof Double)
            assertEquals((Double) o, val);
        else
            assertEquals(o, (Object) val);
    }

    public static ExprArray toArray(Object... args) {
        return Exprs.toArray(args);
    }

    public static ExprVariable var(String var) throws ExprException {
        Range r = Range.valueOf(var);
        if (r != null)
            var = r.toString();
        ExprVariable v = new ExprVariable(new BasicEvaluationCallback(), var);
        v.setAnnotation(r);
        return v;
    }

    public static ExprArray loadArray(String filename) throws IOException {
        ExprArrayBuilder ab = new ExprArrayBuilder();
        for (String line : IO.readLines(TH.class, filename)) {
            ab.addRow(Exprs.parseValues(CSV.parseLine(line, ',', false)));
        }
        return ab.toArray();
    }
}
