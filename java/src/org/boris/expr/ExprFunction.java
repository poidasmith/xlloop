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

public class ExprFunction extends ExprEvaluatable
{
    private IEvaluationCallback callback;
    private String name;
    private Expr[] args;

    public ExprFunction(IEvaluationCallback callback, String name, Expr[] args) {
        super(ExprType.Function);
        this.callback = callback;
        this.name = name;
        this.args = args;
    }

    public String getName() {
        return name;
    }

    public int size() {
        return args.length;
    }

    public Expr getArg(int index) {
        return args[index];
    }

    public Expr evaluate() throws ExprException {
        return callback.evaluateFunction(name, args);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(name);
        sb.append("(");
        for (int i = 0; i < args.length; i++) {
            if (i > 0)
                sb.append(",");
            sb.append(args[i]);
        }
        sb.append(")");
        return sb.toString();
    }
}
