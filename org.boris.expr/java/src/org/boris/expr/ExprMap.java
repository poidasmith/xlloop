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

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class ExprMap extends Expr
{
    private Map<String, Expr> values;

    public ExprMap(Map<String, Expr> values) {
        super(ExprType.Map, false);
        this.values = values;
    }
    
    public ExprMap() {
        this(new LinkedHashMap());
    }
    
    public Set<String> keys() {
        return values.keySet();
    }
    
    public Expr get(String key) {
        return values.get(key);
    }
    
    public void put(String key, Expr value) {
        values.put(key, value);
    }
    
    public void put(String key, String value) {
        values.put(key, new ExprString(value));
    }    
    
    public void put(String key, double value) {
        values.put(key, new ExprDouble(value));
    }
    
    public void put(String key, int value) {
        values.put(key, new ExprInteger(value));
    }
    
    public void put(String key, boolean value) {
        values.put(key, new ExprBoolean(value));
    }
    
    public ExprArray toArray() {
        ExprArray a = new ExprArray(values.size(), 2);
        int idx = 0;
        for(String key : values.keySet()) {
            a.set(idx, 0, new ExprString(key));
            a.set(idx, 1, values.get(key));
            idx++;
        }
        return a;
    }
}
