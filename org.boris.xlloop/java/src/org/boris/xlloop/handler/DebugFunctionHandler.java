/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.handler;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.FunctionHandler;
import org.boris.xlloop.RequestException;

public class DebugFunctionHandler implements FunctionHandler
{
    private FunctionHandler h;

    public DebugFunctionHandler(FunctionHandler h) {
        this.h = h;
    }

    public Variant execute(String name, VTCollection args) throws RequestException {
        System.out.println(name + args);
        return h.execute(name, args);
    }

    public boolean hasFunction(String name) {
        return h.hasFunction(name);
    }

}
