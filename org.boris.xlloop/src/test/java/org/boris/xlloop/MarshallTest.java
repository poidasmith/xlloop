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

public class MarshallTest extends TestCase
{
    XLoperObjectConverter xlo = new XLoperObjectConverter();
    XLArray a = new XLArray(2, 2);
    {
        a.set(0, 0, "string");
        a.set(0, 1, true);
        a.set(1, 0, 3.2);
        a.set(1, 1, 45);
    }

    public void testObject2D() throws Exception {
        Object[][] o = (Object[][]) xlo.createFrom(a, Object[][].class);
        assertEquals(o[0][0], "string");
        assertEquals(o[0][1], Boolean.TRUE);
        assertEquals(o[1][0], new Double(3.2));
        assertEquals(o[1][1], new Integer(45));
    }

    public void testXLArray() throws Exception {
        XLArray a2 = (XLArray) xlo.createFrom(a, XLArray.class);
        assertEquals(a, a2);
    }

    public void testDouble2D() throws Exception {
        Double[][] d = (Double[][]) xlo.createFrom(a, Double[][].class);
        assertEquals(d[0][0], null);
        assertEquals(d[0][1], new Double(1));
        assertEquals(d[1][0], new Double(3.2));
        assertEquals(d[1][1], new Double(45));
    }

    public void testPrimitiveDouble2D() throws Exception {
        double[][] d2 = (double[][]) xlo.createFrom(a, double[][].class);
        assertEquals(d2[0][0], 0., 0);
        assertEquals(d2[0][1], 1., 0);
        assertEquals(d2[1][0], 3.2, 0);
        assertEquals(d2[1][1], 45., 0);
    }

    public void testInteger2D() throws Exception {
        Integer[][] i1 = (Integer[][]) xlo.createFrom(a, Integer[][].class);
        assertEquals(i1[0][0], null);
        assertEquals(i1[0][1], new Integer(1));
        assertEquals(i1[1][0], new Integer(3));
        assertEquals(i1[1][1], new Integer(45));
    }

    public void testPrimitiveInt2D() throws Exception {
        int[][] i2 = (int[][]) xlo.createFrom(a, int[][].class);
        assertEquals(i2[0][0], 0);
        assertEquals(i2[0][1], 1);
        assertEquals(i2[1][0], 3);
        assertEquals(i2[1][1], 45);
    }

    public void testObjectVector() throws Exception {
        Object[] o2 = (Object[]) xlo.createFrom(a, Object[].class);
        assertEquals(o2.length, 4);
        assertEquals(o2[0], "string");
        assertEquals(o2[1], Boolean.TRUE);
        assertEquals(o2[2], new Double(3.2));
        assertEquals(o2[3], new Integer(45));
    }

    public void testPrimitiveDoubleVector() throws Exception {
        double[] d = (double[]) xlo.createFrom(a, double[].class);
        assertEquals(d.length, 4);
        assertEquals(d[0], 0., 0);
        assertEquals(d[1], 1., 0);
        assertEquals(d[2], 3.2, 0);
        assertEquals(d[3], 45., 0);
    }

    public void testDoubleVector() throws Exception {
        Double[] d = (Double[]) xlo.createFrom(a, Double[].class);
        assertEquals(d.length, 4);
        assertEquals(d[0], null);
        assertEquals(d[1], new Double(1.));
        assertEquals(d[2], new Double(3.2));
        assertEquals(d[3], new Double(45.));
    }

    public void testIntegerVector() throws Exception {
        Integer[] d = (Integer[]) xlo.createFrom(a, Integer[].class);
        assertEquals(d.length, 4);
        assertEquals(d[0], null);
        assertEquals(d[1], new Integer(1));
        assertEquals(d[2], new Integer(3));
        assertEquals(d[3], new Integer(45));
    }

    public void testIntegerPrimitiveVector() throws Exception {
        int[] d = (int[]) xlo.createFrom(a, int[].class);
        assertEquals(d.length, 4);
        assertEquals(d[0], 0);
        assertEquals(d[1], 1);
        assertEquals(d[2], 3);
        assertEquals(d[3], 45);
    }

    public void testSingle() throws Exception {
        assertEquals("string", xlo.createFrom(a, Object.class));
        assertEquals(null, xlo.createFrom(a, double.class));
        assertEquals(null, xlo.createFrom(a, Integer.class));
    }
}
