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

import java.util.HashMap;
import java.util.Map;

import org.boris.expr.engine.EngineProvider;
import org.boris.expr.engine.GridReference;
import org.boris.expr.engine.Range;
import org.boris.expr.function.excel.SUM;

public class BasicEngineProvider implements EngineProvider
{
    static private Map<String, IExprFunction> functions = new HashMap();
    static {
        functions.put("SUM", new SUM());
        functions.put("TESTRANGE", new FunctionTestRange());
    }

    private int columns = 100;
    private int rows = 100;

    public void inputChanged(Range range, Expr input) {
        System.out.println("Input changed: " + range + "=\n" + input);
    }

    public void valueChanged(Range range, Expr value) {
        System.out.println("Value changed: " + range + "=\n" + value);
    }

    public void validate(ExprVariable variable) throws ExprException {
        Range r = (Range) variable.getAnnotation();
        if (r == null) {
            r = Range.valueOf(variable.getName());
        }

        validate(r.getDimension1());
        validate(r.getDimension2());
    }

    private void validate(GridReference ref) throws ExprException {
        if (ref.getColumn() < 1 || ref.getColumn() > columns)
            throw new ExprException("Invalid column: " + ref.getColumn());
        if (ref.getRow() < 1 || ref.getRow() > rows)
            throw new ExprException("Invalid row: " + ref.getRow());
    }

    public Expr evaluateFunction(ExprFunction function) throws ExprException {
        IExprFunction f = functions.get(function.getName().toUpperCase());
        if (f == null)
            throw new ExprException("Unknown function: " + function.getName());
        return f.evaluate(function.getArgs());
    }

    public Expr evaluateVariable(ExprVariable variable) throws ExprException {
        return null;
    }
}
