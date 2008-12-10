/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr;

import junit.framework.TestCase;

import org.boris.expr.engine.GridReference;
import org.boris.expr.engine.Range;

public class RangeTest extends TestCase
{
    public void test1() {
        assertGrid(GridReference.valueOf("S$1"), 19, false, 1, true);
        assertNull(GridReference.valueOf("1"));
        assertGrid(GridReference.valueOf("AA32"), 27, false, 32, false);
        assertNull(GridReference.valueOf("A$A32"));
        assertGrid(GridReference.valueOf("$B5666"), 2, true, 5666, false);
        assertNull(GridReference.valueOf("B"));
        assertGrid(GridReference.valueOf("121313s1"), -512202195, false, 1,
                false);
        assertNull(GridReference.valueOf(""));
        assertNull(GridReference.valueOf("   "));
        assertGrid(GridReference.valueOf("$A$1"), 1, true, 1, true);
    }

    public void testRange() throws Exception {
        assertEquals(Range.valueOf("Sheet1!A5:$G$7"), new Range("Sheet1",
                new GridReference(1, 5), new GridReference(7, true, 7, true)));
        assertEquals(Range.valueOf("A5:$G$7"), new Range(null,
                new GridReference(1, 5), new GridReference(7, true, 7, true)));
        assertEquals(Range.valueOf("$G$7"), new Range(null, new GridReference(
                7, true, 7, true), null));
        assertFalse(Range.valueOf("$G$7").equals(
                new Range(null, new GridReference(7, false, 7, true), null)));
        testInvalidRange("!asdf");
        testInvalidRange("Sheet!!");
    }

    public void testSplit() throws Exception {
        assertEquals(4, Range.valueOf("A1:B2").split().length);
        assertEquals(new GridReference(1, 1), Range.valueOf("A1:B2").split()[0]
                .getDimension1());
    }

    private static void testInvalidRange(String r) {
        try {
            assertNull(Range.valueOf(r));
        } catch (Exception ex) {
        }
    }

    public static void out(GridReference gf) {
        Object o = gf;
        if (gf != null) {
            System.out.println(gf);
            o = gf.encode();
        }
        System.out.println(o);
    }

    private void assertGrid(GridReference g, int column, boolean columnFixed,
            int row, boolean rowFixed) {
        assertEquals(g, new GridReference(column, columnFixed, row, rowFixed));
    }
}
