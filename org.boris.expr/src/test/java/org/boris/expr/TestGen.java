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

import org.boris.expr.function.excel.ZTEST;

public class TestGen
{
    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(
                ZTEST.class.getResourceAsStream("flist.txt")));
        String line = null;
        while ((line = br.readLine()) != null) {
            System.out.println("public void test" + line +
                    "() throws Exception {");
            System.out.println("    " + line + " " +
                    Character.toLowerCase(line.charAt(0)) + " = new " + line +

                    "();");
            System.out.println("    fail(\"" + line + " not implemented\");");
            System.out.println("}");
        }
    }
}
