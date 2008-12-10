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

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.boris.expr.function.ExcelFunctionProvider;
import org.boris.expr.function.FunctionManager;
import org.boris.expr.parser.ExprLexer;
import org.boris.expr.parser.ExprParser;

public class BasicEvaluationCallback implements IEvaluationCallback
{
    private Map<String, Expr> variables = new HashMap();
    private FunctionManager functions = new FunctionManager();

    public BasicEvaluationCallback() {
        functions.add(new ExcelFunctionProvider());
        variables.put("TRUE", ExprBoolean.TRUE);
        variables.put("FALSE", ExprBoolean.FALSE);
    }

    public void addVariable(String name, Expr value) {
        variables.put(name, value);
    }

    public void addFunction(String name, IExprFunction function) {
        functions.add(name, function);
    }

    public Expr evaluateFunction(ExprFunction function) throws ExprException {
        return functions.evaluateFunction(function);
    }

    public Expr evaluateVariable(ExprVariable variable) throws ExprException {
        return variables.get(variable.getName());
    }

    public Expr parse(String expr) throws IOException, ExprException {
        ExprParser ep = new ExprParser();
        ep.parse(new ExprLexer(expr), this);
        return ep.get();
    }
}
