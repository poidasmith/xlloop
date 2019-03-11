/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.util;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.boris.xlloop.xloper.XLArray;
import org.boris.xlloop.xloper.XLBool;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLNum;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

public class XLMap
{
    private Map<String, XLoper> map = new LinkedHashMap();

    public XLMap() {
    }

    public XLMap(XLArray array) {
        fromXloper(array);
    }

    public void add(String name, XLoper value) {
        map.put(name, value);
    }

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

    public void add(String name, boolean value) {
        map.put(name, new XLBool(value));
    }

    public String getString(String name) {
        XLoper x = map.get(name);
        if (x instanceof XLString)
            return ((XLString) x).str;
        if (x == null)
            return null;
        return x.toString();
    }

    public Double getDouble(String name) {
        XLoper x = map.get(name);
        if (x instanceof XLNum)
            return ((XLNum) x).num;
        else if (x instanceof XLInt)
            return (double) ((XLInt) x).w;
        else if (x instanceof XLBool)
            return ((XLBool) x).bool ? 1. : 0.;
        return null;
    }

    public double getDouble(String name, double defult) {
        Double d = getDouble(name);
        return d == null ? defult : d.doubleValue();
    }

    public Integer getInt(String name) {
        Double d = getDouble(name);
        return d != null ? d.intValue() : null;
    }

    public int getInt(String name, int defult) {
        return (int) getDouble(name, defult);
    }

    public Boolean getBool(String name) {
        Double d = getDouble(name);
        return d != null ? d.intValue() != 0 : null;
    }

    public boolean getBool(String name, boolean defult) {
        return getInt(name, defult ? 1 : 0) != 0;
    }

    public Set<String> keySet() {
        return map.keySet();
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
        return toXloper().toString();
    }
}
