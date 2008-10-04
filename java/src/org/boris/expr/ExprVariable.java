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

import org.boris.variant.VTMap;
import org.boris.variant.Variant;

public class ExprVariable extends ExprEvaluatable
{
    private IEvaluationCallback callback;
    private String name;

    public ExprVariable(IEvaluationCallback callback, String name) {
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

    public Variant encode() {
        VTMap m = new VTMap();
        m.add("type", type.toString());
        m.add("name", name);
        return m;
    }
}
