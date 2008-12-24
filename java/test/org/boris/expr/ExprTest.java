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

import org.boris.expr.parser.ExprParser;

public class ExprTest extends TH
{
    public void test1() throws Exception {
        assertResult("1+2", 3.);
        testParse("sin(4.3)");
        testParse("8-sin((3/2)+4)");
        testParse("echo(\"asd\")");
        testParse("printf(\"asdf\", 34, 45.6, x)");
        testParse("1 +    5 / 45 - (  sin (sdf) )");
    }

    public void test2() throws Exception {
        testParse("1+2+3-5");
        assertResult("1+2/5*2", 1.8);
        assertResult("1+2/(8*(4+1))", 1.05);
        assertResult(c(), "1-sum(1,2)+x", 3.3);
    }

    public void testUnary() throws Exception {
        assertResult("3-(-5)*2", 13.);
        assertException("*5");
        assertResult("-3", -3.);
    }

    public void testExpressions() throws Exception {
        assertResult("((2))", 2);
        assertResult("(4)*(3/4)", 3.);
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
        c.set("A1", "Hello ");
        c.set("A2", "World!");
        assertResult(c, "A1&A2", "Hello World!");
        assertResult(c, "A1&(A1&A2)", "Hello Hello World!");
        assertResult(c, "A1&A1&A2", "Hello Hello World!");
        assertResult(c, "\"A\"&\"B\"&\"C\"", "ABC");
        assertResult(c, "\"\"\"\"", "\"");
        assertResult(c, "\"asdf\"\"sdf\"", "asdf\"sdf");
    }

    private void testParse(String expr) throws Exception {
        Expr e = ExprParser.parse(expr, null);
        System.out.println(e.encode());
    }

    private BasicEvaluationCallback c() {
        BasicEvaluationCallback c = new BasicEvaluationCallback();
        c.addVariable("X", new ExprDouble(5.3));
        return c;
    }
}
