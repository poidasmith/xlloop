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

import org.boris.xlloop.xloper.XLArray;
import org.boris.xlloop.xloper.XLBool;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLNil;
import org.boris.xlloop.xloper.XLNum;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

/**
 * Used to map arguments to objects.
 */
public class XLoperObjectConverter
{
    public static final Integer IZERO = new Integer(0);
    public static final Double DZERO = new Double(0);
    public static final Long LZERO = new Long(0);
    private ObjectRegistry registry = new ObjectRegistry();

    /**
     * Clear out the internal registry to release memory.
     */
    public void clearRegistry() {
        registry.clear();
    }

    /**
     * Get the size of the registry.
     * 
     * @return int.
     */
    public int getRegistrySize() {
        return registry.size();
    }

    /**
     * Creates an Variant from a java object (best attempt).
     * 
     * @param obj.
     * 
     * @return XLObject.
     */
    public XLoper createFrom(Object obj) {
        if (obj instanceof String) {
            return new XLString((String) obj);
        } else if (obj instanceof Boolean) {
            return new XLBool(((Boolean) obj).booleanValue());
        } else if (obj instanceof Integer) {
            return new XLNum(((Integer) obj).intValue());
        } else if (obj instanceof Float) {
            return new XLNum(((Float) obj).floatValue());
        } else if (obj instanceof Double) {
            return new XLNum(((Double) obj).doubleValue());
        } else if (obj instanceof String[]) {
            String[] arr = (String[]) obj;
            XLoper[] array = new XLoper[arr.length];
            for (int i = 0; i < arr.length; i++) {
                array[i] = new XLString(arr[i]);
            }

            return new XLArray(array, arr.length, 1);
        } else if (obj instanceof String[][]) {
            String[][] arr = (String[][]) obj;
            XLArray array = new XLArray(arr.length, arr[0].length);

            for (int i = 0; i < arr.length; i++) {
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    array.set(i, j, arr[i][j]);
                }
            }

            return array;
        } else if (obj instanceof double[]) {
            double[] arr = (double[]) obj;
            XLoper[] array = new XLoper[arr.length];

            for (int i = 0; i < arr.length; i++) {
                array[i] = new XLNum(arr[i]);
            }

            return new XLArray(array, array.length, 1);
        } else if (obj instanceof double[][]) {
            double[][] arr = (double[][]) obj;
            XLArray array = new XLArray(arr.length, arr[0].length);

            for (int i = 0; i < arr.length; i++) {
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    array.set(i, j, arr[i][j]);
                }
            }

            return array;
        } else if (obj instanceof Double[]) {
            Double[] arr = (Double[]) obj;
            XLoper[] array = new XLoper[arr.length];

            for (int i = 0; i < arr.length; i++) {
                array[i] = arr[i] == null ? (XLoper) XLNil.NIL : new XLNum(
                        arr[i].doubleValue());
            }

            return new XLArray(array, arr.length, 1);
        } else if (obj instanceof Double[][]) {
            Double[][] arr = (Double[][]) obj;
            XLArray array = new XLArray(arr.length, arr[0].length);

            for (int i = 0; i < arr.length; i++) {
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    array.set(i, j, arr[i][j]);
                }
            }

            return array;
        } else if (obj instanceof int[]) {
            int[] arr = (int[]) obj;
            XLoper[] array = new XLoper[arr.length];

            for (int i = 0; i < arr.length; i++) {
                array[i] = new XLInt(arr[i]);
            }

            return new XLArray(array, arr.length, 1);
        } else if (obj instanceof int[][]) {
            int[][] arr = (int[][]) obj;
            XLArray array = new XLArray(arr.length, arr[0].length);

            for (int i = 0; i < arr.length; i++) {
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    array.set(i, j, arr[i][j]);
                }
            }

            return array;
        } else if (obj instanceof Integer[]) {
            Integer[] arr = (Integer[]) obj;
            XLoper[] array = new XLoper[arr.length];

            for (int i = 0; i < arr.length; i++) {
                array[i] = arr[i] == null ? (XLoper) XLNil.NIL : new XLInt(
                        arr[i].intValue());
            }

            return new XLArray(array, arr.length, 1);
        } else if (obj instanceof Integer[][]) {
            Integer[][] arr = (Integer[][]) obj;
            XLArray array = new XLArray(arr.length, arr[0].length);

            for (int i = 0; i < arr.length; i++) {
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    array.set(i, j, arr[i][j]);
                }
            }

            return array;
        } else if (obj instanceof boolean[]) {
            boolean[] arr = (boolean[]) obj;
            XLoper[] array = new XLoper[arr.length];

            for (int i = 0; i < arr.length; i++) {
                array[i] = new XLBool(arr[i]);
            }

            return new XLArray(array, arr.length, 1);
        } else if (obj instanceof boolean[][]) {
            boolean[][] arr = (boolean[][]) obj;
            XLArray array = new XLArray(arr.length, arr[0].length);

            for (int i = 0; i < arr.length; i++) {
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    array.set(i, j, arr[i][j]);
                }
            }

            return array;
        } else if (obj instanceof Boolean[]) {
            Boolean[] arr = (Boolean[]) obj;
            XLoper[] array = new XLoper[arr.length];

            for (int i = 0; i < arr.length; i++) {
                array[i] = arr[i] == null ? (XLoper) XLNil.NIL : new XLBool(
                        arr[i].booleanValue());
            }

            return new XLArray(array, arr.length, 1);
        } else if (obj instanceof Boolean[][]) {
            Boolean[][] arr = (Boolean[][]) obj;
            XLArray array = new XLArray(arr.length, arr[0].length);

            for (int i = 0; i < arr.length; i++) {
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    array.set(i, j, arr[i][j]);
                }
            }

            return array;
        } else if (obj instanceof Object[][]) {
            Object[][] arr = (Object[][]) obj;
            XLArray array = new XLArray(arr.length, arr[0].length);

            for (int i = 0; i < arr.length; i++) {
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    array.set(i, j, createFrom(arr[i][j]));
                }
            }

            return array;
        } else if (obj instanceof Object[]) {
            Object[] arr = (Object[]) obj;
            XLoper[] array = new XLoper[arr.length];

            for (int i = 0; i < arr.length; i++) {
                array[i] = createFrom(arr[i]);
            }

            return new XLArray(array, arr.length, 1);
        } else if (obj instanceof XLoper) {
            return (XLoper) obj;
        } else if (obj != null) {
            return new XLString(registry.put(obj));
        } else {
            return XLNil.NIL;
        }
    }

    /**
     * Creates a java object from an XLObject.
     * 
     * @param obj.
     * @param hint.
     * 
     * @return Object.
     */
    public Object createFrom(XLoper obj, Class hint) {
        switch (obj.type) {
        case XLoper.xlTypeStr:
            if (XLString.class.equals(hint)) {
                return obj;
            } else {
                String str = ((XLString) obj).str;
                Object val = registry.get(str);
                if (val != null) {
                    return val;
                } else {
                    return str;
                }
            }
        case XLoper.xlTypeNum:
            if (String.class.equals(hint)) {
                return obj.toString();
            } else if (Integer.class.equals(hint) || int.class.equals(hint)) {
                return new Integer((int) ((XLNum) obj).num);
            } else if (Long.class.equals(hint) || long.class.equals(hint)) {
                return new Long((long) ((XLNum) obj).num);
            } else if (Boolean.class.equals(hint) || boolean.class.equals(hint)) {
                return new Boolean(((int) ((XLNum) obj).num) != 0);
            } else {
                return new Double(((XLNum) obj).num);
            }
        case XLoper.xlTypeMulti:
            return convertArray((XLArray) obj, hint);
        case XLoper.xlTypeBool:
            if (Double.class.equals(hint) || double.class.equals(hint)) {
                return new Double(((XLBool) obj).bool ? 1 : 0);
            } else if (String.class.equals(hint)) {
                return obj.toString();
            } else if (Integer.class.equals(hint) || int.class.equals(hint)) {
                return new Integer(((XLBool) obj).bool ? 1 : 0);
            } else if (Long.class.equals(hint) || long.class.equals(hint)) {
                return new Long(((XLBool) obj).bool ? 1 : 0);
            } else {
                return new Boolean((((XLBool) obj).bool));
            }
        case XLoper.xlTypeInt:
            if (Double.class.equals(hint) || double.class.equals(hint)) {
                return new Double(((XLInt) obj).w);
            } else if (String.class.equals(hint)) {
                return obj.toString();
            } else if (Long.class.equals(hint) || long.class.equals(hint)) {
                return new Long(((XLInt) obj).w);
            } else if (Boolean.class.equals(hint) || boolean.class.equals(hint)) {
                return new Boolean((((XLInt) obj).w != 0));
            } else {
                return new Integer(((XLInt) obj).w);
            }
        case XLoper.xlTypeNil:
        case XLoper.xlTypeMissing:
            if (String.class.equals(hint)) {
                return "";
            } else if (int.class.equals(hint)) {
                return IZERO;
            } else if (long.class.equals(hint)) {
                return LZERO;
            } else if (boolean.class.equals(hint)) {
                return Boolean.FALSE;
            } else if (double.class.equals(hint)) {
                return DZERO;
            } else {
                return null;
            }
        }

        return null;
    }

    private Object convertVector(XLArray arr, Class hint) {
        Object val = null;

        if (Integer.class.equals(hint)) {
            Integer l = arr.length > 0 ? arr.getInteger(0) : null;
            return l == null ? null : new Integer(l.intValue());
        } else if (int.class.equals(hint)) {
            Integer l = arr.length > 0 ? arr.getInteger(0) : null;
            return l == null ? IZERO : new Integer(l.intValue());
        } else if (Double.class.equals(hint)) {
            return arr.length > 0 ? arr.getDouble(0) : null;
        } else if (double.class.equals(hint)) {
            Double d = arr.length > 0 ? arr.getDouble(0) : null;
            return d == null ? DZERO : d;
        } else if (String.class.equals(hint)) {
            return arr.length > 0 ? arr.getString(0) : null;
        } else if (double[].class.equals(hint)) {
            double[] darr = new double[arr.rows];

            for (int i = 0; i < darr.length; i++) {
                Double d = arr.getDouble(i);
                if (d != null) {
                    darr[i] = d.doubleValue();
                }
            }

            val = darr;
        } else if (double[][].class.equals(hint)) {
            double[][] darr = new double[arr.rows][1];

            for (int i = 0; i < arr.rows; i++) {
                Double d = arr.getDouble(i);
                if (d != null) {
                    darr[i][0] = d.doubleValue();
                }
            }

            val = darr;
        } else if (Double[].class.equals(hint)) {
            Double[] darr = new Double[arr.rows];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getDouble(i);
            }

            val = darr;
        } else if (Double[][].class.equals(hint)) {
            Double[][] darr = new Double[arr.rows][1];

            for (int i = 0; i < arr.rows; i++) {
                darr[i][0] = arr.getDouble(i);
            }

            val = darr;
        } else if (int[].class.equals(hint)) {
            int[] darr = new int[arr.rows];

            for (int i = 0; i < darr.length; i++) {
                Integer l = arr.getInteger(i);
                if (l != null) {
                    darr[i] = l.intValue();
                }
            }

            val = darr;
        } else if (int[][].class.equals(hint)) {
            int[][] darr = new int[arr.rows][1];

            for (int i = 0; i < arr.rows; i++) {
                Integer l = arr.getInteger(i);
                if (l != null) {
                    darr[i][0] = l.intValue();
                }
            }

            val = darr;
        } else if (Integer[].class.equals(hint)) {
            Integer[] darr = new Integer[arr.rows];

            for (int i = 0; i < darr.length; i++) {
                Integer l = arr.getInteger(i);
                if (l != null) {
                    darr[i] = l;
                }
            }

            val = darr;
        } else if (Integer[][].class.equals(hint)) {
            Integer[][] darr = new Integer[arr.rows][1];

            for (int i = 0; i < arr.rows; i++) {
                Integer l = arr.getInteger(i);
                if (l != null) {
                    darr[i][0] = l;
                }
            }

            val = darr;
        } else if (boolean[].class.equals(hint)) {
            boolean[] darr = new boolean[arr.rows];

            for (int i = 0; i < darr.length; i++) {
                Integer l = arr.getInteger(i);
                if (l != null) {
                    darr[i] = l.intValue() == 1;
                }
            }

            val = darr;
        } else if (boolean[][].class.equals(hint)) {
            boolean[][] darr = new boolean[arr.rows][1];

            for (int i = 0; i < arr.rows; i++) {
                Integer l = arr.getInteger(i);
                if (l != null) {
                    darr[i][0] = l.intValue() == 1;
                }
            }

            val = darr;
        } else if (Boolean[].class.equals(hint)) {
            Boolean[] darr = new Boolean[arr.rows];

            for (int i = 0; i < darr.length; i++) {
                Integer l = arr.getInteger(i);
                if (l != null) {
                    darr[i] = new Boolean(l.intValue() == 1);
                }
            }

            val = darr;
        } else if (Boolean[][].class.equals(hint)) {
            Boolean[][] darr = new Boolean[arr.rows][1];

            for (int i = 0; i < arr.rows; i++) {
                Integer l = arr.getInteger(i);
                if (l != null) {
                    darr[i][0] = new Boolean(l.intValue() == 1);
                }
            }

            val = darr;
        } else if (String[].class.equals(hint)) {
            String[] darr = new String[arr.rows];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getString(i);
            }

            val = darr;
        } else if (String[][].class.equals(hint)) {
            String[][] darr = new String[arr.rows][1];

            for (int i = 0; i < arr.rows; i++) {
                darr[i][0] = arr.getString(i);
            }

            val = darr;
        } else if (Object[].class.equals(hint)) {
            Object[] darr = new Object[arr.rows];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = createFrom(arr.get(i), Object.class);
            }

            val = darr;
        } else if (Object[][].class.equals(hint)) {
            Object[][] darr = new Object[arr.rows][1];

            for (int i = 0; i < arr.rows; i++) {
                darr[i][0] = createFrom(arr.get(i), Object.class);
            }

            val = darr;
        } else {
            String str = arr.getString(0);
            val = registry.get(str);
        }

        return val;
    }

    /**
     * Convert an array into the desired object.
     * 
     * @param arr.
     * @param hint.
     * 
     * @return Object.
     */
    private Object convertArray(XLArray arr, Class hint) {
        Object val = null;
        if (arr.columns == 1) {
            return convertVector(arr, hint);
        }

        if (Integer.class.equals(hint) || int.class.equals(hint)) {
            val = arr.getInteger(0);
        } else if (Double.class.equals(hint) || double.class.equals(hint)) {
            val = arr.getDouble(0);
        } else if (String.class.equals(hint)) {
            val = arr.getString(0);
        } else if (double[].class.equals(hint)) {
            double[] darr = new double[arr.rows * arr.columns];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getDouble(i).doubleValue();
            }

            val = darr;
        } else if (double[][].class.equals(hint)) {
            double[][] darr = new double[arr.rows][arr.columns];

            for (int i = 0; i < arr.rows; i++) {
                for (int j = 0; j < arr.columns; j++) {
                    Double d = arr.getDouble(i, j);
                    if (d != null)
                        darr[i][j] = d.doubleValue();
                }
            }

            val = darr;
        } else if (Double[].class.equals(hint)) {
            Double[] darr = new Double[arr.rows * arr.columns];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getDouble(i);
            }

            val = darr;
        } else if (Double[][].class.equals(hint)) {
            Double[][] darr = new Double[arr.rows][arr.columns];

            for (int i = 0; i < arr.rows; i++) {
                for (int j = 0; j < arr.columns; j++) {
                    darr[i][j] = arr.getDouble(i, j);
                }
            }

            val = darr;
        } else if (int[].class.equals(hint)) {
            int[] darr = new int[arr.rows * arr.columns];

            for (int i = 0; i < darr.length; i++) {
                Integer it = arr.getInteger(i);
                if (it != null)
                    darr[i] = it.intValue();
            }

            val = darr;
        } else if (int[][].class.equals(hint)) {
            int[][] darr = new int[arr.rows][arr.columns];

            for (int i = 0; i < arr.rows; i++) {
                for (int j = 0; j < arr.columns; j++) {
                    Integer it = arr.getInteger(i, j);
                    if (it != null)
                        darr[i][j] = it.intValue();
                }
            }

            val = darr;
        } else if (Integer[].class.equals(hint)) {
            Integer[] darr = new Integer[arr.rows * arr.columns];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getInteger(i);
            }

            val = darr;
        } else if (Integer[][].class.equals(hint)) {
            Integer[][] darr = new Integer[arr.rows][arr.columns];

            for (int i = 0; i < arr.rows; i++) {
                for (int j = 0; j < arr.columns; j++) {
                    darr[i][j] = arr.getInteger(i, j);
                }
            }

            val = darr;
        } else if (boolean[].class.equals(hint)) {
            boolean[] darr = new boolean[arr.rows * arr.columns];

            for (int i = 0; i < darr.length; i++) {
                Boolean b = arr.getBoolean(i);
                if (b != null)
                    darr[i] = b.booleanValue();
            }

            val = darr;
        } else if (boolean[][].class.equals(hint)) {
            boolean[][] darr = new boolean[arr.rows][arr.columns];

            for (int i = 0; i < arr.rows; i++) {
                for (int j = 0; j < arr.columns; j++) {
                    Boolean b = arr.getBoolean(i, j);
                    if (b != null)
                        darr[i][j] = b.booleanValue();
                }
            }

            val = darr;
        } else if (Boolean[].class.equals(hint)) {
            Boolean[] darr = new Boolean[arr.rows * arr.columns];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getBoolean(i);
            }

            val = darr;
        } else if (Boolean[][].class.equals(hint)) {
            Boolean[][] darr = new Boolean[arr.rows][arr.columns];

            for (int i = 0; i < arr.rows; i++) {
                for (int j = 0; j < arr.columns; j++) {
                    darr[i][j] = arr.getBoolean(i, j);
                }
            }

            val = darr;
        } else if (String[].class.equals(hint)) {
            String[] darr = new String[arr.rows * arr.columns];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getString(i);
            }

            val = darr;
        } else if (String[][].class.equals(hint)) {
            String[][] darr = new String[arr.rows][arr.columns];

            for (int i = 0; i < arr.rows; i++) {
                for (int j = 0; j < arr.columns; j++) {
                    darr[i][j] = arr.getString(i, j);
                }
            }

            val = darr;
        } else if (Object[].class.equals(hint)) {
            Object[] darr = new Object[arr.rows * arr.columns];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = createFrom(arr.get(i), Object.class);
            }

            val = darr;
        } else if (Object[][].class.equals(hint)) {
            Object[][] darr = new Object[arr.rows][arr.columns];

            for (int i = 0; i < arr.rows; i++) {
                for (int j = 0; j < arr.columns; j++) {
                    darr[i][j] = createFrom(arr.get(i, j), Object.class);
                }
            }

            val = darr;
        } else if (XLArray.class.equals(hint)) {
            val = arr;
        } else {
            String str = arr.getString(0);
            val = registry.get(str);
        }

        return val;
    }

    public Object[] convert(XLoper[] args, Class[] hints) {
        Object[] a = new Object[hints.length];
        int csize = args.length;
        for (int i = 0; i < hints.length; i++) {
            if (i < csize) {
                a[i] = createFrom(args[i], hints[i]);
            } else {
                a[i] = null;
            }
        }
        return a;
    }
}
