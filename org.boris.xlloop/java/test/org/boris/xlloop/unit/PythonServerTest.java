/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.unit;

import org.boris.xlloop.framework.XLLoopTestCase;
import org.boris.xlloop.util.XLSparseArray;
import org.boris.xlloop.xloper.XLoper;

public class PythonServerTest extends XLLoopTestCase
{
    public void testArrays() throws Exception {
        XLSparseArray xpa = new XLSparseArray();
        xpa.set(0, 0, Math.PI);
        xpa.set(0, 1, "test");
        xpa.set(1, 0, 10);
        // xpa.set(1, 1, true);
        XLoper res = execute("ArgsTest", xpa.toXLoper());
        System.out.println(res);
    }
}
