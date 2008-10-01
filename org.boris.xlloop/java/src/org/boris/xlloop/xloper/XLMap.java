/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.xloper;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

public class XLMap
{
    private Map map = new LinkedHashMap();

    public void add(String name, String value) {
        map.put(name, new XLString(value));
    }

    public void add(String name, int value) {
        map.put(name, new XLInt(value));
    }

    public void add(String name, double value) {
        map.put(name, new XLNum(value));
    }

    public void add(String name, XLList value) {
        map.put(name, value.toXLoper());
    }

    public void fromXloper(XLArray array) {
        if (array.columns != 2) {
            return;
        }

        for (int i = 0; i < array.length; i += 2) {
            String key = array.array[i].toString();
            XLoper value = array.array[i + 1];
            map.put(key, value);
        }
    }

    public XLoper toXloper() {
        int size = map.size() << 1;
        XLoper[] data = new XLoper[size];
        Iterator iter = map.keySet().iterator();
        for (int i = 0; i < size; i += 2) {
            String key = (String) iter.next();
            data[i] = new XLString(key);
            data[i + 1] = (XLoper) map.get(key);
        }
        return new XLArray(data, size >> 1, 2);
    }

    public String toString() {
        return map.toString();
    }
}
