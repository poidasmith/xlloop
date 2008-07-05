/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop;

import java.util.HashMap;
import java.util.Map;

import org.boris.variant.VTCollection;
import org.boris.variant.Variant;

public class FunctionMap implements FunctionHandler 
{
    private Map functions = new HashMap();

    public void add(String name, Function f) {
        this.functions.put(name, f);
    }

    public void remove(String name) {
        this.functions.remove(name);
    }

    public void clear() {
        this.functions.clear();
    }

    public Variant execute(String name, VTCollection args) throws RequestException {
        Function f = (Function) functions.get(name);
        if (f == null) {
            throw new RequestException("Unknown function: " + name);
        }
        return f.execute(args);
    }

    public boolean hasFunction(String name) {
        return functions.containsKey(name);
    }
}
