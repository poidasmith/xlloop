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

import org.boris.xlloop.menu.CommandResult;
import org.boris.xlloop.menu.IMenuProvider;
import org.boris.xlloop.menu.Menu;
import org.boris.xlloop.menu.MenuItem;
import org.boris.xlloop.menu.SubMenu;

public class TestMenu implements IMenuProvider
{
    public CommandResult execute(String item, String submenu) {
        return new CommandResult();
    }

    public Menu getMenu() {
        Menu m = new Menu("&Sheets");
        // m.add(MenuItem.SEPARATOR);
        m.add(new MenuItem("&Basic Data"));
        m.add(new MenuItem("Test&3"));
        m.add(new MenuItem("Test&4"));
        m.add(new MenuItem("Test&5"));
        m.add(new MenuItem("Test&6"));
        SubMenu sub1 = new SubMenu("&Sub1", 5);
        sub1.add(new MenuItem("Sub Test&1"));
        sub1.add(new MenuItem("Sub Test&2"));
        sub1.add(new MenuItem("Sub Test&3"));
        m.add(sub1);
        return m;
    }
}
