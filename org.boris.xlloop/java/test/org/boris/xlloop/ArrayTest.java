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

        // Test generic conversion
        Object[][] o = (Object[][]) xlo.createFrom(a, Object[][].class);
        assertEquals(o[0][0], "string");
        assertEquals(o[0][1], Boolean.TRUE);
        assertEquals(o[1][0], new Double(3.2));
        assertEquals(o[1][1], new Integer(45));

        // Test XLoper conversion
        XLArray a2 = (XLArray) xlo.createFrom(a, XLArray.class);
        assertEquals(a, a2);

        // Test double object conversion
        Double[][] d = (Double[][]) xlo.createFrom(a, Double[][].class);
        assertEquals(d[0][0], null);
        assertEquals(d[0][1], new Double(1));
        assertEquals(d[1][0], new Double(3.2));
        assertEquals(d[1][1], new Double(45));

        // Test double primitive conversion
        double[][] d2 = (double[][]) xlo.createFrom(a, double[][].class);
        assertEquals(d2[0][0], 0.);
        assertEquals(d2[0][1], 1.);
        assertEquals(d2[1][0], 3.2);
        assertEquals(d2[1][1], 45.);

        // Test integer object conversion
        Integer[][] i1 = (Integer[][]) xlo.createFrom(a, Integer[][].class);
        assertEquals(i1[0][0], null);
        assertEquals(i1[0][1], new Integer(1));
        assertEquals(i1[1][0], new Integer(3));
        assertEquals(i1[1][1], new Integer(45));

        // Test integer primitive conversion
        int[][] i2 = (int[][]) xlo.createFrom(a, int[][].class);
        assertEquals(i2[0][0], 0);
        assertEquals(i2[0][1], 1);
        assertEquals(i2[1][0], 3);
        assertEquals(i2[1][1], 45);
    }
}
