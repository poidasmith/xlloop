/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.jxll.util;

import java.lang.reflect.Field;

public class Reflection
{
    public static String toString(Object o) {
        if (o == null)
            return "null";
        StringBuilder sb = new StringBuilder();
        Field[] fields = o.getClass().getDeclaredFields();
        for (int fi = 0; fi < fields.length; fi++) {
            Field f = fields[fi];
            f.setAccessible(true);
            sb.append(f.getName());
            sb.append(": ");
            try {
                Object val = f.get(o);
                if (val instanceof Integer) {
                    sb.append(f.get(o));
                    sb.append(" (0x");
                    sb.append(Integer.toHexString(((Integer) val).intValue()));
                    sb.append(")");
                } else if (val instanceof Long) {
                    sb.append(f.get(o));
                    sb.append(" (0x");
                    sb.append(Long.toHexString(((Long) val).longValue()));
                    sb.append(")");
                } else if (val != null && val.getClass().isArray()) {
                    if (val instanceof int[]) {
                        int[] arr = (int[]) val;
                        for (int i = 0; i < arr.length && i < 10; i++) {
                            if (i != 0)
                                sb.append(", ");
                            sb.append(arr[i]);
                        }
                    } else if (val instanceof byte[]) {
                        byte[] arr = (byte[]) val;
                        for (int i = 0; i < arr.length && i < 10; i++) {
                            if (i != 0)
                                sb.append(", ");
                            sb.append(Integer.toHexString(arr[i] & 0xff));
                        }
                    } else {
                        Object[] arr = (Object[]) val;
                        for (int i = 0; i < arr.length && i < 10; i++) {
                            if (i != 0)
                                sb.append(", ");
                            sb.append(arr[i]);
                        }
                    }
                } else {
                    sb.append(f.get(o));
                }
            } catch (Exception e) {
                sb.append(e.getMessage());
            }
            sb.append("\n");
        }
        return sb.toString();
    }
}
