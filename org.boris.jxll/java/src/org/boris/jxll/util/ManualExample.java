/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.jxll.util;

import org.boris.jxll.Addin;
import org.boris.jxll.FunctionInformation;
import org.boris.jxll.JXLL;
import org.boris.jxll.XLOper;

/**
 * Demonstrates loading a regular DLL and manually registering functions.
 */
public class ManualExample
{
    private static String ADDIN = "F:/Development/xll/xllsdk97/SAMPLES/ADVDLL/ADVDLL.DLL";

    public static void main(String[] args) throws Exception {
        // Load the addin
        Addin a = JXLL.load(ADDIN);

        // Register the double arg function
        a.registerFunction(new FunctionInformation("DoubleArg", "BB"));

        // Call the double arg function
        XLOper res = a.invoke("DoubleArg", new Double(2.));

        // Display the result
        System.out.println(res.num);

        // Free the addin
        JXLL.dispose(a);
    }
}
