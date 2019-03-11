/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.reflect;

import java.lang.reflect.Field;

/**
 * Provides a wrapper for the Reflection API to be used in excel.
 */
public class Reflect
{
    public static String[][] getFields(String clazz) throws Exception {
        Field[] fields = Class.forName(clazz).getDeclaredFields();
        String[][] result = new String[fields.length][2];
        for (int i = 0; i < fields.length; i++) {
            result[i][0] = fields[i].getName();
            result[i][1] = fields[i].getType().getCanonicalName();
        }
        return result;
    }

    public static String getClassName(Object obj) {
        return obj == null ? "null" : obj.getClass().getCanonicalName();
    }

    public static Object getField(Object obj, String name) throws Exception {
        return obj.getClass().getField(name).get(obj);
    }
}
