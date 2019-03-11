/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr.engine;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.boris.expr.Expr;
import org.boris.expr.ExprBoolean;
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprError;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprInteger;
import org.boris.expr.ExprString;
import org.boris.expr.ExprVariable;
import org.boris.expr.IExprFunction;
import org.boris.expr.function.ExcelFunctionProvider;
import org.boris.expr.function.FunctionManager;
import org.boris.expr.function.IFunctionProvider;
import org.boris.expr.graph.Edge;
import org.boris.expr.graph.Graph;
import org.boris.expr.graph.GraphCycleException;
import org.boris.expr.parser.ExprParser;

/**
 * Simple graph-based dependency engine.
 */
public class SimpleGraphEngine
{
    private EngineFunctionManager evaluator = new EngineFunctionManager();
    private Graph<String> graph = new Graph();
    private Map<String, Expr> inputs = new HashMap<String, Expr>();
    private Map<String, Expr> results = new HashMap<String, Expr>();
    private Set<IEngineListener> listeners = new HashSet<IEngineListener>();

    public SimpleGraphEngine() {
        addFunctions(new ExcelFunctionProvider());
        setValue("PI", ExprDouble.PI);
        setValue("TRUE", ExprBoolean.TRUE);
        setValue("FALSE", ExprBoolean.FALSE);
    }

    public void addEngineListener(IEngineListener listener) {
        listeners.add(listener);
    }

    public void addFunction(String name, IExprFunction function) {
        evaluator.add(name, function);
    }

    public void addFunctions(IFunctionProvider provider) {
        evaluator.add(provider);
    }

    public void setValue(String name, String value) {
        setValue(name, new ExprString(value));
    }

    public void setValue(String name, double value) {
        setValue(name, new ExprDouble(value));
    }

    public void setValue(String name, int value) throws GraphCycleException {
        setValue(name, new ExprInteger(value));
    }

    public void setValue(String name, boolean value) {
        setValue(name, new ExprBoolean(value));
    }

    public void set(String name, Expr value) throws GraphCycleException {
        setValue(name, value);
        for (ExprVariable v : ExprVariable.findVariables(value)) {
            graph.add(new Edge<String>(v.getName(), name));
        }
    }

    public void setFormula(String name, String expression) throws ExprException, GraphCycleException {
        set(name, parse(expression));
    }

    public void calculate() throws ExprException {
        for (String name : graph.sort()) {
            Expr input = inputs.get(name);
            if (input instanceof ExprEvaluatable) {
                Expr result = null;
                try {
                    fireBeforeCalculation(name);
                    result = ((ExprEvaluatable) input).evaluate();
                } catch (ExprException e) {
                    result = new ExprError(e);
                }
                results.put(name, result);
                fireAfterCalculation(name, result);
            }
        }
    }

    public Expr get(String name) {
        return results.get(name);
    }

    public Expr parse(String expression) throws ExprException {
        try {
            return ExprParser.parse(expression, evaluator).optimize();
        } catch (IOException e) {
            throw new ExprException(e);
        }
    }

    public void clear(String name) {
        inputs.remove(name);
        results.remove(name);
        graph.remove(name);
    }

    private void setValue(String name, Expr value) {
        inputs.put(name, value);
        if (value instanceof ExprEvaluatable)
            results.remove(name);
        else {
            results.put(name, value);
            fireAfterCalculation(name, value);
        }
        graph.clearInbounds(name);
        graph.add(name);
    }

    private void fireBeforeCalculation(String name) {
        for (IEngineListener l : listeners)
            l.beforeCalculation(name);
    }

    private void fireAfterCalculation(String name, Expr value) {
        for (IEngineListener l : listeners)
            l.afterCalculation(name, value);
    }

    private class EngineFunctionManager extends FunctionManager
    {
        public Expr evaluateVariable(ExprVariable variable) throws ExprException {
            Expr v = results.get(variable.getName());
            return v == null ? ExprError.NAME : v;
        }
    }
}
