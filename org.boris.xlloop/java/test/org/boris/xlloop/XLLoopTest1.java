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

import org.boris.xlloop.util.XLList;
import org.boris.xlloop.xloper.XLNum;

public class XLLoopTest1 extends XLLoopTestCase
{
    private FunctionServer server;

    protected void setUp() throws Exception {
        server = ServerTest1.createServer();
        server.start();
        super.setUp();
    }

    protected void tearDown() throws Exception {
        server.stop();
        super.tearDown();
    }

    public void test1() throws Exception {
        // System.out.println(executeFunction("org.boris.xlloop.GetFunctions",
        // new XLList()));
        System.out.println(executeFunction("Math.sin", XLList.make(
                new XLNum(Math.PI / 2)).toArray()));
    }
}
