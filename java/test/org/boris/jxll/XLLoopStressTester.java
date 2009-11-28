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

public class XLLoopStressTester implements IXLLHost
{
    private XLOper caller;
    private String sheet;

    public static void main(String[] args) throws Exception {
        XLLoopStressTester t = new XLLoopStressTester();
        XLL.setCallback(t);
        Addin xlloop = XLL.load("../WinRun4J/build/XLLoop-Debug/XLLoop-Debug.xll");
        for (int i = 0; i < 10000; i++) {
            XLOper res = xlloop.invoke("FS", new XLOper[] { new XLOper("Math.random") }, true);
            System.out.println(res);
        }
    }

    public int excel4(int xlfn, XLOperHolder operRes, XLOper[] opers) {
        return 0;
    }

    public int xlCallVer() {
        return 0;
    }
}
