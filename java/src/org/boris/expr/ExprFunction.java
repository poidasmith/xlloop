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

import org.boris.variant.VTCollection;
import org.boris.variant.VTMap;
import org.boris.variant.Variant;

public class ExprFunction extends ExprEvaluatable
{
    private IEvaluationCallback callback;
    private String name;
    private Expr[] args;
    private Object annotation;

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

    public Expr[] getArgs() {
        return args;
    }

    public void setAnnotation(Object annotation) {
        this.annotation = annotation;
    }

    public Object getAnnotation() {
        return annotation;
    }

    public Expr evaluate() throws ExprException {
        return callback.evaluateFunction(this);
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

    public Variant encode() {
        VTMap m = new VTMap();
        m.add("type", type.toString());
        m.add("name", name);
        VTCollection collection = new VTCollection();
        for (int i = 0; i < args.length; i++) {
            collection.add(args[i].encode());
        }
        m.add("args", collection);
        return m;
    }

    public void validate() throws ExprException {
        if (name == null)
            throw new ExprException("Function name cannot be empty");
        for (int i = 0; i < args.length; i++) {
            args[i].validate();
        }
    }
}
