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

public class GridReferenceTest extends TestCase
{
    public void test1() throws Exception {
        assertEquals(GridReference.toColumnIndex("A"), 1);
        assertEquals(GridReference.toColumnIndex("B"), 2);
        assertEquals(GridReference.toColumnIndex("F"), 6);
        assertEquals(GridReference.toColumnIndex("AA"), 27);
        assertEquals(GridReference.toColumnIndex("BG"), 59);
        assertEquals(GridReference.toColumnIndex("ZZZ"), 18278);
    }

    public void test2() throws Exception {
        assertEquals("A", GridReference.toColumnName(1));
        assertEquals("E", GridReference.toColumnName(5));
        assertEquals("M", GridReference.toColumnName(13));
        assertEquals("BG", GridReference.toColumnName(59));
        assertEquals("ZZZ", GridReference.toColumnName(18278));
        assertEquals("Z", GridReference.toColumnName(26));
    }
}
