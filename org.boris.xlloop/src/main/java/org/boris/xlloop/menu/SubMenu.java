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

public class SubMenu
{
    private String name;
    private Integer position;
    private List<MenuItem> items = new ArrayList();

    public SubMenu(String name) {
        this.name = name;
    }

    public SubMenu(String name, Integer position) {
        this.name = name;
        this.position = position;
    }

    public void add(MenuItem item) {
        items.add(item);
    }

    public String getName() {
        return name;
    }

    public Integer getPosition() {
        return position;
    }

    public int size() {
        return items.size();
    }

    public MenuItem get(int index) {
        return items.get(index);
    }
}
