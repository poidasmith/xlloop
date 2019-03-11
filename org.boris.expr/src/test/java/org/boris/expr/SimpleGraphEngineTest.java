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

import org.boris.expr.engine.SimpleGraphEngine;

public class SimpleGraphEngineTest extends TH
{
    public void testEngine1() throws Exception {
        SimpleGraphEngine e = new SimpleGraphEngine();
        e.setValue("x", 3.);
        e.setValue("y", 5.);
        e.setFormula("z", "x+y+2");
        e.setFormula("a", "z/2");
        e.setFormula("b", "a^2");
        e.setFormula("y", "13+5");
        e.calculate();
        assertEquals(e.get("a"), 5.);
    }
}
