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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * Used to contain objects and provides mapping to a generated key.
 */
public class ObjectRegistry
{
    private Map map = new HashMap();
    private Map rev = new HashMap();
    private Random rand = new Random(new Date().getTime());
    private SimpleDateFormat format = new SimpleDateFormat(
            "EEE, d MMM yyyy HH:mm:ss:SSS");

    /**
     * Place an object in the registry.
     * 
     * @param obj.
     * 
     * @return String (the generated key).
     */
    public String put(Object obj) {
        if (rev.containsKey(obj)) {
            return (String) rev.get(obj);
        } else {
            String key = createKey(obj);
            map.put(key, obj);
            rev.put(obj, key);
            return key;
        }
    }

    /**
     * Get the keys.
     * 
     * @return String[].
     */
    public String[] getKeys() {
        return (String[]) map.keySet().toArray(new String[0]);
    }

    /**
     * Retrieve an object from the registry.
     * 
     * @param key.
     * 
     * @return Object.
     */
    public Object get(String key) {
        return map.get(key);
    }

    /**
     * Remove an object from the registry.
     * 
     * @param key.
     */
    public void remove(String key) {
        Object o = map.get(key);
        if (o != null) {
            map.remove(key);
            rev.remove(o);
        }
    }

    /**
     * Generate the key for the object.
     * 
     * @param obj.
     * 
     * @return String.
     */
    private String createKey(Object obj) {
        StringBuffer sb = new StringBuffer();
        sb.append((obj == null) ? "null" : obj.getClass().getName());
        sb.append("@");

        synchronized (format) {
            sb.append(format.format(new Date()));
        }

        sb.append("..");
        sb.append(Math.abs(rand.nextLong()));

        return sb.toString();
    }

    /**
     * Clear out the registry.
     */
    public void clear() {
        map.clear();
        rev.clear();
    }

    /**
     * Get the size of the registry.
     * 
     * @return int.
     */
    public int size() {
        return map.size();
    }
}
