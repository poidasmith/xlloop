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

import java.util.LinkedHashSet;
import java.util.Set;

import org.boris.expr.Expr;
import org.boris.expr.ExprException;
import org.boris.expr.ExprFunction;
import org.boris.expr.ExprVariable;
import org.boris.expr.IEvaluationCallback;
import org.boris.expr.IExprFunction;

public class FunctionManager implements IEvaluationCallback, IFunctionProvider
{
    private FunctionMap functionMap;
    private Set<IFunctionProvider> providers = new LinkedHashSet();

    public FunctionManager() {
        this(false);
    }

    public FunctionManager(boolean caseSensitive) {
        functionMap = new FunctionMap(caseSensitive);
        providers.add(functionMap);
    }

    public void add(String name, IExprFunction function) {
        functionMap.add(name, function);
    }

    public void add(IFunctionProvider provider) {
        functionMap.add(provider);
    }

    public Expr evaluateFunction(ExprFunction function) throws ExprException {
        return functionMap.evaluate(function);
    }

    public Expr evaluateVariable(ExprVariable variable) throws ExprException {
        return null;
    }

    public boolean hasFunction(ExprFunction function) {
        return functionMap.hasFunction(function);
    }

    public Expr evaluate(ExprFunction function) throws ExprException {
        return evaluateFunction(function);
    }
}
