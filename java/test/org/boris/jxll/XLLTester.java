/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.jxll;

import org.boris.jxll.util.Reflection;

public class XLLTester
{
    public static void main(String[] args) throws Exception {
        Addin test = XLL.load("jni\\build\\TestXLL-Debug\\TestXLL.dll");
        XLOper s = test.invoke("TestSum", new Double(30), new Double(12));
        System.out.println(Reflection.toString(s));
        XLOper t = test.invoke("TestChar", new Integer(32), "Sample",
                new Double(5.5), new Boolean(true));
        System.out.println(Reflection.toString(t));
        XLOper u = test.invoke("TestArgs", new Integer(32), "Echo Please",
                new Boolean(true), new Integer(7), new Integer(8));
        System.out.println(Reflection.toString(u));
        // XLL.load("F:/Development/xll/EXAMPLE.xll");
        // XLL.load("F:/Development/xll/generic.xll");
        // XLL.load("F:\\Downloads\\xllsdk97\\SAMPLES\\FRAMEWRK\\generic.xll");
    }
}
