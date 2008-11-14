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

import java.util.ArrayList;
import java.util.List;

import org.boris.variant.VTMap;
import org.boris.variant.Variant;

public class ExprVariable extends ExprEvaluatable
{
    private IEvaluationCallback callback;
    private String name;
    private Object annotation;

    public ExprVariable(IEvaluationCallback callback, String name) {
        super(ExprType.Variable);
        this.callback = callback;
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setAnnotation(Object annotation) {
        this.annotation = annotation;
    }

    public Object getAnnotation() {
        return annotation;
    }

    public Expr evaluate() throws ExprException {
        return callback.evaluateVariable(name);
    }

    public String toString() {
        return name;
    }

    public Variant encode() {
        VTMap m = new VTMap();
        m.add("type", type.toString());
        m.add("name", name);
        return m;
    }

    public static ExprVariable[] findVariables(Expr expr) {
        List<ExprVariable> vars = new ArrayList();
        findVariables(expr, vars);
        return vars.toArray(new ExprVariable[0]);
    }

    public static void findVariables(Expr expr, List<ExprVariable> vars) {
        if (expr instanceof ExprFunction) {
            ExprFunction f = (ExprFunction) expr;
            for (int i = 0; i < f.size(); i++) {
                findVariables(f.getArg(i), vars);
            }
        } else if (expr instanceof ExprExpression) {
            findVariables(((ExprExpression) expr).getChild(), vars);
        } else if (expr instanceof IBinaryOperator) {
            IBinaryOperator bo = (IBinaryOperator) expr;
            findVariables(bo.getLHS(), vars);
            findVariables(bo.getRHS(), vars);
        } else if (expr instanceof ExprVariable) {
            vars.add(((ExprVariable) expr));
        }
    }

    public void validate() throws ExprException {
        if (name == null)
            throw new ExprException("Variable name is empty");
    }
}
