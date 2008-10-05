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

import java.io.BufferedReader;
import java.io.InputStreamReader;

import junit.framework.TestCase;

import org.boris.expr.parser.ExprLexer;
import org.boris.expr.parser.ExprParser;

public class ExprTest extends TestCase
{
    public void Qtest1() throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(getClass()
                .getResourceAsStream("test.txt")));
        String line = null;
        while ((line = br.readLine()) != null) {
            test(line);
        }
    }

    public void test2() throws Exception {
        // test("1+2+3-5");
        testEval("1+2/5*2", 1.8);
        testEval("1+2/(8*(4+1))", 1.05);
        testEval("1-sum(1,2)+x", 3.3);
    }

    public void testExpressions() throws Exception {
        testEval("((2))", 2);
        testEval("(4)*(3/4)", 3);
    }

    public void testFunctions() throws Exception {
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
            public Expr evaluateFunction(String name, Expr[] args)
                    throws ExprException {
                return new ExprDouble(5.5);
            }

            public Expr evaluateVariable(String name) throws ExprException {
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
