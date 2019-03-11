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

import java.util.ArrayList;
import java.util.List;

public class Menu
{
    private String name;
    private List<MenuItem> items = new ArrayList();
    private List<SubMenu> submenus = new ArrayList();

    public Menu(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void add(MenuItem item) {
        items.add(item);
    }

    public void add(SubMenu sub) {
        submenus.add(sub);
    }

    public int getItemCount() {
        return items.size();
    }

    public MenuItem getItem(int index) {
        return items.get(index);
    }

    public int getSubMenuCount() {
        return submenus.size();
    }

    public SubMenu getSubMenu(int index) {
        return submenus.get(index);
    }
}
