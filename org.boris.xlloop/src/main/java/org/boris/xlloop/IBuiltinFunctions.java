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

public interface IBuiltinFunctions
{
    /**
     * Called by the addin when creating a new session (ie. upon connection)
     */
    String INITIALIZE = "org.boris.xlloop.Initialize";

    /**
     * Used by the addin to get a collection of functions to register
     */
    String GET_FUNCTIONS = "org.boris.xlloop.GetFunctions";

    /**
     * Used by the addin to get a menu (including submenus)
     */
    String GET_MENU = "org.boris.xlloop.GetMenu";

    /**
     * Called by the addin when executing a command
     */
    String EXEC_COMMAND = "org.boris.xlloop.ExecuteCommand";
}
