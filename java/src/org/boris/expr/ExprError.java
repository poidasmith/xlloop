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

public class ExprError extends Expr
{
    private String errType;
    private String message;

    public ExprError(String type, String message) {
        super(ExprType.Error, false);
        this.errType = type;
        this.message = message;
    }

    public String getErrType() {
        return errType;
    }

    public String getMessage() {
        return message;
    }

    public String toString() {
        return "#" + message;
    }

    public Variant encode() {
        VTMap m = new VTMap();
        m.add("type", type.toString());
        m.add("errType", errType);
        m.add("message", message);
        return m;
    }
}
