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

import junit.framework.TestCase;

import org.boris.expr.parser.ExprLexer;
import org.boris.expr.parser.ExprParser;

public class ExprTest extends TestCase
{
    public void test1() throws Exception {
        testEval("1+2", 3);
        test("sin(4.3)");
        test("8-sin((3/2)+4)");
        test("echo(\"asd\")");
        test("printf(\"asdf\", 34, 45.6, x)");
        test("1 +    5 / 45 - (  sin (sdf) )");
    }

    public void test2() throws Exception {
        test("1+2+3-5");
        testEval("1+2/5*2", 1.8);
        testEval("1+2/(8*(4+1))", 1.05);
        testEval("1-sum(1,2)+x", 3.3);
    }

    public void testUnary() throws Exception {
        testEval("3-(-5)*2", 13);
        // assertException("*5");
        // assertResult("-3", -3);
    }

    public void testExpressions() throws Exception {
        testEval("((2))", 2);
        testEval("(4)*(3/4)", 3);
    }

    public void testVariables() throws Exception {
        testParse("A1");
        testParse("$A1");
        testParse("A$1");
        testParse("$A$1");
        testParse("Sheet1!A1");
        testParse("Sheet1!$A1");
        testParse("Sheet1!A$1");
        testParse("Sheet1!$A$1");
    }

    public void testStrings() throws Exception {
        BasicEvaluationCallback c = new BasicEvaluationCallback();
        c.addVariable("A1", new ExprString("Hello "));
        c.addVariable("A2", new ExprString("World!"));
        assertResult(c, "A1&A2", new ExprString("Hello World!"));
        assertResult(c, "A1&(A1&A2)", new ExprString("Hello Hello World!"));
        assertResult(c, "A1&A1&A2", new ExprString("Hello Hello World!"));
        assertResult(c, "\"A\"&\"B\"&\"C\"", new ExprString("ABC"));
        assertResult(c, "\"\"\"\"", new ExprString("\""));
        assertResult(c, "\"asdf\"\"sdf\"", new ExprString("asdf\"sdf"));
    }

    private void testParse(String expr) throws Exception {
        Expr e = ExprParser.parse(expr, null);
        System.out.println(e.encode());
    }

    private void assertResult(BasicEvaluationCallback c, String expression,
            Expr result) throws Exception {
        System.out.println(expression);
        Expr e = c.parse(expression);
        if (e.evaluatable) {
            e = ((ExprEvaluatable) e).evaluate();
        }
        assertEquals(e, result);
    }

    private void testEval(String line, double expected) throws Exception {
        ExprNumber n = (ExprNumber) test(line);
        assertEquals(expected, n.doubleValue());
    }

    private Expr test(String line) throws Exception {
        System.out.println();
        System.out.println(line);
        ExprLexer l = new ExprLexer(line);
        ExprParser p = new ExprParser();
        p.parse(l, new IEvaluationCallback() {
            public Expr evaluateFunction(ExprFunction funtion)
                    throws ExprException {
                return new ExprDouble(5.5);
            }

            public Expr evaluateVariable(ExprVariable variable)
                    throws ExprException {
                return new ExprDouble(7.8);
            }
        });
        Expr out = p.get();
        System.out.println(out);
        System.out.println(out.encode());
        if (out instanceof ExprEvaluatable) {
            out = ((ExprEvaluatable) out).evaluate();
            System.out.println(out.encode());
        }
        return out;
    }
}
