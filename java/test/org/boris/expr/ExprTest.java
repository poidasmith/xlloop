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
import org.boris.expr.parser.ExprToken;

public class ExprTest extends TestCase
{
    public void test1() throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(getClass()
                .getResourceAsStream("test.txt")));
        String line = null;
        while ((line = br.readLine()) != null) {
            test(line);
        }
    }

    private void test(String line) throws Exception {
        System.out.println();
        System.out.println(line);
        ExprLexer l = new ExprLexer(line);
        ExprToken token = null;
        while ((token = l.next()) != null) {
            System.out.println(token);
        }
    }
}
