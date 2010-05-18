/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.menu;

import org.boris.xlloop.IFunctionContext;
import org.boris.xlloop.IFunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

public class MenuHandler implements IFunctionHandler
{
    public static final String AF_GET_MENU = "org.boris.xlloop.GetMenu";
    public static final String AF_EXEC_COMMAND = "org.boris.xlloop.ExecuteCommand";

    private IMenuProvider provider;

    public MenuHandler(IMenuProvider provider) {
        this.provider = provider;
    }

    public XLoper execute(IFunctionContext context, String name, XLoper[] args) throws RequestException {
        if (AF_EXEC_COMMAND.equals(name)) {
            String item = null;
            String submenu = null;
            return provider.execute(item, submenu).toXLoper();
        } else if (AF_GET_MENU.equals(name)) {
            return MenuCodec.toXLoper(provider.getMenu());
        }
        return new XLString("#Unknown function: " + name);
    }

    public boolean hasFunction(String name) {
        return AF_EXEC_COMMAND.equals(name) || AF_GET_MENU.equals(name);
    }
}
