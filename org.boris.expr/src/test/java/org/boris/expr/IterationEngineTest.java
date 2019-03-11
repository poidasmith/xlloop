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

import org.boris.expr.engine.AbstractCalculationEngine;
import org.boris.expr.engine.IterationEngine;
import org.boris.expr.engine.Range;

public class IterationEngineTest extends TestCase
{
    public void testBasic() throws Exception {
        IterationEngine e = new IterationEngine(new BasicEngineProvider());
        e.set("B1", "=A1*2");
        e.set("A1", "=12*2");
        e.set("C1", "=B1*A1");
        assertResult(e, "B1", 48);
        assertResult(e, "C1", 48 * 24);
        e.set("A1", "2");
        assertResult(e, "B1", 4);
        assertResult(e, "C1", 8);
    }

    public void testRangeDependencies() throws Exception {
        IterationEngine e = new IterationEngine(new BasicEngineProvider());
        e.set("A1", "1");
        e.set("B1", "2");
        e.set("A2", "3");
        e.set("B2", "4");
        e.set("D4", "=sum(A1:B2)");
        assertResult(e, "D4", 10);
        e.set("A1", "10");
        assertResult(e, "D4", 19);
    }

    public void testFunction() throws Exception {
        IterationEngine e = new IterationEngine(new BasicEngineProvider());
        e.set("A4", "25");
        e.set("B6", "26");
        e.set("A1", "=sum(45,34,2.3,A4:B8)");
        assertResult(e, "A1", 132.3);
    }

    public void testArrays() throws Exception {
        IterationEngine e = new IterationEngine(new BasicEngineProvider());
        e.set("A1:B2", "32");
        assertResult(e, "A2", 32);
        e.set("D5:E6", "=TestRange(A1)");
    }

    public void testAliases() throws Exception {
        IterationEngine e = new IterationEngine(new BasicEngineProvider());
        e.setNamespace("Sheet1");
        e.addAlias("x", Range.valueOf("A1:B3"));
        e.addAlias("alias1", Range.valueOf("A1"));
        e.set("A1", "45");
        e.set("B1", "=alias1:x");
        e.set("B2", "=x");
        assertResult(e, "B1", 45);
        assertResult(e, "B2", 45);
    }

    public void testInvalidReference() throws Exception {
        IterationEngine e = new IterationEngine(new BasicEngineProvider());
        assertException(e, "B1:A1", "12");
    }

    public void testCircular() throws Exception {
        IterationEngine e = new IterationEngine(new BasicEngineProvider());
    }

    public void testManualCalculate() throws Exception {
        IterationEngine e = new IterationEngine(new BasicEngineProvider());
        e.setAutoCalculate(false);
    }

    private void assertException(AbstractCalculationEngine e, String range,
            String expression) throws Exception {
        try {
            e.set(range, expression);
            fail("Expected an exception");
        } catch (Exception ex) {
        }
    }

    private void assertResult(AbstractCalculationEngine e, String range,
            double value) throws Exception {
        Expr expr = e.getValue(Range.valueOf(range));
        if (expr instanceof ExprNumber) {
            assertEquals(value, ((ExprNumber) expr).doubleValue());
        } else {
            fail("Range: " + range + " does not contain a number");
        }
    }
}
