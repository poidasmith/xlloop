/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.http;

import org.boris.xlloop.xloper.XLSRef;
import org.boris.xlloop.xloper.XLoper;

public class FunctionRequest
{
    private String name;
    private XLoper[] args;
    private XLSRef caller;
    private String sheetName;

    public FunctionRequest(String name, XLoper[] args, XLSRef caller, String sheetName) {
        this.name = name;
        this.args = args;
        this.caller = caller;
        this.sheetName = sheetName;
    }

    public String getName() {
        return name;
    }

    public XLoper[] getArgs() {
        return args;
    }

    public XLSRef getCaller() {
        return caller;
    }

    public String getSheetName() {
        return sheetName;
    }
}
