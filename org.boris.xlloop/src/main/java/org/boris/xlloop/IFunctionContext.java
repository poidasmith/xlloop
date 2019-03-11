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

import org.boris.xlloop.xloper.XLSRef;
import org.boris.xlloop.xloper.XLoper;

public interface IFunctionContext
{
    String getUser();

    String getHost();

    String getUserKey();

    XLSRef getCaller();

    String getSheetName();

    XLoper execute(String name, XLoper[] args) throws RequestException;
}
