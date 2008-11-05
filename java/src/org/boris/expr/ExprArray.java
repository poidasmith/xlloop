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

import org.boris.variant.Variant;

public class ExprArray extends Expr
{
    private int columns;
    private Expr[][] array;

    ExprArray(int rows, int columns) {
        super(ExprType.Array, false);
        this.array = new Expr[rows][];
        this.columns = columns;
    }

    public Variant encode() {
        return null;
    }
}
