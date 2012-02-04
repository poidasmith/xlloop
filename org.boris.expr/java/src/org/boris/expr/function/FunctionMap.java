/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr.function;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.boris.expr.Expr;
import org.boris.expr.ExprException;
import org.boris.expr.ExprFunction;
import org.boris.expr.IExprFunction;

public class FunctionMap implements IFunctionProvider
{
    private Map<String, IExprFunction> functions;

    public FunctionMap() {
        this(false);
    }

    public FunctionMap(boolean caseSensitive) {
        functions = caseSensitive ? new HashMap() : new TreeMap(String.CASE_INSENSITIVE_ORDER);
    }

    public void add(String name, IExprFunction function) {
        functions.put(name, function);
    }

    public Expr evaluate(ExprFunction function) throws ExprException {
        IExprFunction f = functions.get(function.getName());
        if (f != null)
            return f.evaluate(function.getArgs());
        return null;
    }

    public boolean hasFunction(ExprFunction function) {
        return functions.containsKey(function.getName());
    }
}
