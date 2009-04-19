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

import org.boris.xlloop.util.XLoperObjectConverter;
import org.boris.xlloop.xloper.XLArray;

public class ArrayTest extends TestCase
{
    public void testArrays() throws Exception {
        XLoperObjectConverter xlo = new XLoperObjectConverter();
        XLArray a = new XLArray(2, 2);
        a.set(0, 0, "string");
        a.set(0, 1, true);
        a.set(1, 0, 3.2);
        a.set(1, 1, 45);
        Object[][] o = (Object[][]) xlo.createFrom(a, Object[][].class);
        assertEquals(o[0][0], "string");
        assertEquals(o[0][1], Boolean.TRUE);
        assertEquals(o[1][0], new Double(3.2));
        assertEquals(o[1][1], new Integer(45));
        XLArray a2 = (XLArray) xlo.createFrom(a, XLArray.class);
        assertEquals(a, a2);
        Double[][] d = (Double[][]) xlo.createFrom(a, Double[][].class);
        assertEquals(d[0][0], null);
        assertEquals(d[0][1], new Double(1));
        assertEquals(d[1][0], new Double(3.2));
        assertEquals(d[1][1], new Double(45));
    }
}
