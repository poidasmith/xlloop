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

import java.util.Arrays;

import org.boris.variant.VTCollection;
import org.boris.variant.VTMap;
import org.boris.variant.Variant;

public class ExprArray extends Expr
{
    private int columns;
    private int rows;
    private Expr[] array;

    public ExprArray(int rows, int columns) {
        super(ExprType.Array, false);
        this.array = new Expr[rows * columns];
        this.columns = columns;
        this.rows = rows;
    }

    public int rows() {
        return rows;
    }

    public int columns() {
        return columns;
    }

    public int length() {
        return array.length;
    }

    public Expr get(int index) {
        return array[index];
    }

    public Expr get(int row, int column) {
        return array[row * columns + column];
    }

    public void set(int index, Expr value) {
        array[index] = value;
    }

    public void set(int row, int column, Expr value) {
        array[row * columns + column] = value;
    }

    public int hashCode() {
        return 567 ^ rows ^ columns ^ array.length;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof ExprArray))
            return false;

        ExprArray a = (ExprArray) obj;
        return a.rows == rows && a.columns == columns &&
                Arrays.equals(a.array, array);
    }

    public Variant encode() {
        VTMap m = new VTMap();
        m.add("type", type.toString());
        m.add("rows", rows);
        m.add("columns", columns);
        VTCollection values = new VTCollection();
        for (int i = 0; i < array.length; i++) {
            values.add(array[i].encode());
        }
        m.add("values", values);
        return m;
    }
}
