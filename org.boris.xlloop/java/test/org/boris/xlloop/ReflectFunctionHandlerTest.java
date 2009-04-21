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

import org.boris.xlloop.reflect.ReflectFunctionHandler;
import org.boris.xlloop.util.XLoperObjectConverter;
import org.boris.xlloop.xloper.XLoper;

public class ReflectFunctionHandlerTest extends TestCase
{
    XLoperObjectConverter xlo = new XLoperObjectConverter();
    ReflectFunctionHandler rfh = new ReflectFunctionHandler();
    {
        rfh.addMethods("Math.", Math.class);
    }

    public void testSimple() throws Exception {
        assertEquals(new Double(45.4), execute("Math.abs", new Double(45.4)));
        assertEquals(new Double(45.), execute("Math.abs", new Long(45)));
    }

    public Object execute(String name, Object a1) throws Exception {
        return execute(name, new Object[] { a1 });
    }

    public Object execute(String name, Object a1, Object a2) throws Exception {
        return execute(name, new Object[] { a1, a2 });
    }

    public Object execute(String name, Object a1, Object a2, Object a3)
            throws Exception {
        return execute(name, new Object[] { a1, a2, a3 });
    }

    private Object execute(String name, Object[] args) throws Exception {
        XLoper[] x = new XLoper[args.length];
        for (int i = 0; i < x.length; i++) {
            x[i] = xlo.createFrom(args[i]);
        }
        return xlo.createFrom(rfh.execute(name, x), Object.class);
    }
}
