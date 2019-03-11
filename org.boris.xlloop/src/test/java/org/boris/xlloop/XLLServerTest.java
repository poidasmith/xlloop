/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop;

import org.boris.jxll.Addin;
import org.boris.jxll.JXLL;

public class XLLServerTest
{
    public static void main(String[] args) throws Exception {
        Addin a = JXLL.load("F:/Development/xll/XLLserverTest.xll");
        for (int i = 0; i < a.getFunctionCount(); i++) {
            String fn = a.getFunctionName(i);
            System.out.println(fn);
            System.out.println(Reflection.toString(a.getInformation(fn)));
        }
    }
}
