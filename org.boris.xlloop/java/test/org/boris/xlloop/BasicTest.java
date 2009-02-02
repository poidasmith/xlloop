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

import junit.framework.TestCase;

import org.boris.xlloop.util.Maths;
import org.boris.xlloop.xloper.XLNum;
import org.boris.xlloop.xloper.XLoper;

public class BasicTest extends TestCase
{
    public void test1() throws Exception {
        assertEquals(cst.exec("Math.sin", new XLNum(0)), 0.);
    }

    public void assertEquals(XLoper x, double v) {
        assertTrue(x instanceof XLNum);
        assertEquals(((XLNum) x).num, v, 0.000001);
    }

    private ClientServerTester cst;

    protected void setUp() throws Exception {
        cst = new ClientServerTester();
        cst.addMethods("Math.", Maths.class);
        cst.addMethods("Math.", Math.class);
    }

    protected void tearDown() throws Exception {
        cst.stop();
    }
}
