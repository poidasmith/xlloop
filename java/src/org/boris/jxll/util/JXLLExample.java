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
import org.boris.jxll.JXLL;
import org.boris.jxll.XLOper;

public class JXLLExample
{
    public static void main(String[] args) throws Exception {
        System.out.println("Loading TestXLL.dll...");
        Addin a = JXLL.load("TestXLL.dll");
        if (a == null) {
            System.out.println("Failed to load addin");
            return;
        }
        double a1 = Math.round(Math.random() * 60000) / 100.;
        double a2 = Math.round(Math.random() * 4000) / 100.;
        System.out.println("Invoking TestSum(" + a1 + "," + a2 + ")");
        XLOper res = a.invoke("TestSum", new Double(a1), new Double(a2));
        System.out.println(res.num);
    }
}
