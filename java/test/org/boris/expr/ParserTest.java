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

public class ParserTest extends TH
{
    public void testArrays() throws Exception {
        Expr a1 = parse("{1345,1301,1368,1322,1310,1370,1318,1350,1303,1299}");
        Expr a2 = toArray(1345, 1301, 1368, 1322, 1310, 1370, 1318, 1350, 1303,
                1299);
        assertEquals(a1, a2);
    }

    public void testPower() throws Exception {
        assertResult("2^2", 4.);
        assertResult("2^0.5", Math.sqrt(2));
    }
}
