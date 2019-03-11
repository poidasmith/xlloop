/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.command;

import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

public class CommandResult
{
    public XLoper toXLoper() {
        return new XLString("command result testing");
    }
}
