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

import org.boris.xlloop.util.XLList;
import org.boris.xlloop.util.XLMap;
import org.boris.xlloop.xloper.XLoper;

public class MenuCodec
{
    public static XLoper toXLoper(Menu menu) {
        XLMap m = new XLMap();
        XLList items = new XLList();
        XLList subs = new XLList();
        for (int i = 0; i < menu.getItemCount(); i++) {
            items.add(toXLoper(menu.getItem(i)));
        }
        for (int i = 0; i < menu.getSubMenuCount(); i++) {
            subs.add(toXLoper(menu.getSubMenu(i)));
        }
        m.add("name", menu.getName());
        m.add("items", items);
        m.add("submenus", subs);
        return m.toXloper();
    }

    public static XLoper toXLoper(MenuItem item) {
        XLMap m = new XLMap();
        m.add("name", item.getName());
        return m.toXloper();
    }

    public static XLoper toXLoper(SubMenu menu) {
        XLMap m = new XLMap();
        XLList items = new XLList();
        for (int i = 0; i < menu.size(); i++) {
            items.add(toXLoper(menu.get(i)));
        }

        m.add("name", menu.getName());
        if (menu.getPosition() != null)
            m.add("position", menu.getPosition());
        m.add("items", items);
        return m.toXloper();
    }
}
