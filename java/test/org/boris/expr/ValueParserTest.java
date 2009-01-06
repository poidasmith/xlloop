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

import org.boris.expr.util.ValueParser;

public class ValueParserTest extends TH
{
    public void testDates() throws Exception {
        assertEquals(ValueParser.parseDate("3/4/5"), 38415.);
    }
}
