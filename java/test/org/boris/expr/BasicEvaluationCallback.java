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

import org.boris.expr.engine.GridMap;
import org.boris.expr.engine.Range;
import org.boris.expr.function.ExcelFunctionProvider;
import org.boris.expr.function.FunctionManager;
import org.boris.expr.parser.ExprLexer;
import org.boris.expr.parser.ExprParser;
import org.boris.expr.parser.IParserVisitor;
import org.boris.expr.util.Exprs;

public class BasicEvaluationCallback implements IEvaluationCallback,
        IParserVisitor
{
    private Map<String, Expr> variables = new HashMap();
    private FunctionManager functions = new FunctionManager();
    private GridMap grid = new GridMap();

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
        String name = variable.getName().toUpperCase();
        if (variables.containsKey(name)) {
            return variables.get(name);
        }
        Object ann = variable.getAnnotation();
        if (ann instanceof Range) {
            return grid.get((Range) ann);
        }
        Expr e = variables.get(variable.getName().toUpperCase());
        if (e == null)
            return ExprError.NAME;
        return e;
    }

    public Expr parse(String expr) throws IOException, ExprException {
        ExprParser ep = new ExprParser();
        ep.setParserVisitor(this);
        ep.parse(new ExprLexer(expr), this);
        return ep.get();
    }

    public void set(String range, Object value) throws ExprException {
        grid.put(Range.valueOf(range), Exprs.convertObject(value));
    }

    public void set(Range range, Expr value) {
        grid.put(range, value);
    }

    public void set(ExprArray array) {
        set(Range.toRange(array, null), array);
    }

    public void annotateFunction(ExprFunction function) throws ExprException {
    }

    public void annotateVariable(ExprVariable variable) throws ExprException {
        variable.setAnnotation(Range.valueOf(variable.getName()));
    }
}
