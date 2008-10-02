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

public class ExprVariable extends ExprEvaluatable
{
    private IEvaluationCallback callback;
    private String name;

    ExprVariable(IEvaluationCallback callback, String name) {
        super(ExprType.Variable);
        this.callback = callback;
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public Expr evaluate() throws ExprException {
        return callback.evaluateVariable(name);
    }

    public String toString() {
        return name;
    }
}
