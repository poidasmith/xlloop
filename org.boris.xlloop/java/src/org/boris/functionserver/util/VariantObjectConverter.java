package org.boris.functionserver.util;


import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTDouble;
import org.boris.variantcodec.VTLong;
import org.boris.variantcodec.VTNull;
import org.boris.variantcodec.VTString;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;


/**
 * Used to map arguments to objects.
 */
public class VariantObjectConverter {
    private ObjectRegistry registry = new ObjectRegistry();

    /**
     * Clear out the internal registry to release memory.
     */
    public void clearRegistry() {
        registry.clear();
        System.gc();
        Runtime.getRuntime().runFinalization();
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
    public Variant createFrom(Object obj) {
        if (obj instanceof String) {
            return new VTString((String) obj);
        } else if (obj instanceof Integer) {
            return new VTLong(((Integer) obj).intValue());
        } else if (obj instanceof Double) {
            return new VTDouble(((Double) obj).doubleValue());
        } else if (obj instanceof Boolean) {
            return new VTLong(((Boolean) obj).booleanValue());
        } else if (obj instanceof String[]) {
            String[] arr = (String[]) obj;
            VTCollection array = new VTCollection();
            for(int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                r.add(arr[i]);
                array.add(r);
            }

            return array;
        } else if (obj instanceof String[][]) {
            String[][] arr = (String[][]) obj;
            VTCollection array = new VTCollection();

            for (int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    r.add(arr[i][j]);
                }
                array.add(r);
            }

            return array;
        } else if (obj instanceof double[]) {
            double[] arr = (double[]) obj;
            VTCollection array = new VTCollection();
            for(int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                r.add(arr[i]);
                array.add(r);
            }

            return array;
        } else if (obj instanceof double[][]) {
            double[][] arr = (double[][]) obj;
            VTCollection array = new VTCollection();

            for (int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    r.add(arr[i][j]);
                }
                array.add(r);
            }

            return array;
        } else if (obj instanceof Double[]) {
            Double[] arr = (Double[]) obj;
            VTCollection array = new VTCollection();
            for(int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                r.add(arr[i]);
                array.add(r);
            }

            return array;
        } else if (obj instanceof Double[][]) {
            Double[][] arr = (Double[][]) obj;
            VTCollection array = new VTCollection();

            for (int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    r.add(arr[i][j]);
                }
                array.add(r);
            }

            return array;
        } else if (obj instanceof int[]) {
            int[] arr = (int[]) obj;
            VTCollection array = new VTCollection();
            for(int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                r.add(arr[i]);
                array.add(r);
            }

            return array;
        } else if (obj instanceof int[][]) {
            int[][] arr = (int[][]) obj;
            VTCollection array = new VTCollection();

            for (int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    r.add(arr[i][j]);
                }
                array.add(r);
            }

            return array;
        } else if (obj instanceof Integer[]) {
            Integer[] arr = (Integer[]) obj;
            VTCollection array = new VTCollection();
            for(int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                r.add(arr[i]);
                array.add(r);
            }

            return array;
        } else if (obj instanceof Integer[][]) {
            Integer[][] arr = (Integer[][]) obj;
            VTCollection array = new VTCollection();

            for (int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    r.add(arr[i][j]);
                }
                array.add(r);
            }

            return array;
        } else if (obj instanceof boolean[]) {
            boolean[] arr = (boolean[]) obj;
            VTCollection array = new VTCollection();
            for(int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                r.add(arr[i]);
                array.add(r);
            }

            return array;
        } else if (obj instanceof boolean[][]) {
            boolean[][] arr = (boolean[][]) obj;
            VTCollection array = new VTCollection();

            for (int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    r.add(arr[i][j]);
                }
                array.add(r);
            }

            return array;
        } else if (obj instanceof Boolean[]) {
            Boolean[] arr = (Boolean[]) obj;
            VTCollection array = new VTCollection();
            for(int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                r.add(arr[i]);
                array.add(r);
            }

            return array;
        } else if (obj instanceof Boolean[][]) {
            Boolean[][] arr = (Boolean[][]) obj;
            VTCollection array = new VTCollection();

            for (int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    r.add(arr[i][j]);
                }
                array.add(r);
            }

            return array;
        } else if (obj instanceof Object[][]) {
            Object[][] arr = (Object[][]) obj;
            VTCollection array = new VTCollection();

            for (int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                for (int j = 0; j < arr[0].length && j < arr[i].length; j++) {
                    r.add(createFrom(arr[i][j]));
                }
                array.add(r);
            }

            return array;
        } else if (obj instanceof Object[]) {
            Object[] arr = (Object[]) obj;
            VTCollection array = new VTCollection();
            for(int i = 0; i < arr.length; i++) {
                VTCollection r = new VTCollection();
                r.add(createFrom(arr[i]));
                array.add(r);
            }

            return array;
        } else if (obj != null) {
            return new VTString(registry.put(obj));
        } else {
            return VTNull.NULL;
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
    public Object createFrom(Variant obj, Class hint) {
        if(obj instanceof VTString) {
            String str = obj.toString();
            Object val = registry.get(str);
            if(val != null) {
                return val;
            } else {
                return str;
            }
        } else if(obj instanceof VTLong) {
            if (Double.class.equals(hint) || double.class.equals(hint)) {
                return new Double(((VTLong)obj).longValue());
            } else if(String.class.equals(hint)) {
                return obj.toString();
            } else if (Integer.class.equals(hint) || int.class.equals(hint)) {
                return new Integer(((VTLong)obj).intValue());
            } else if (Long.class.equals(hint) || long.class.equals(hint)) {
                return new Long(((VTLong)obj).longValue());
            } else if (Boolean.class.equals(hint) || boolean.class.equals(hint)) {
                return new Boolean(((VTLong)obj).booleanValue());
            }
        } else if(obj instanceof VTDouble) {
            if (Double.class.equals(hint) || double.class.equals(hint)) {
                return new Double(((VTDouble)obj).longValue());
            } else if(String.class.equals(hint)) {
                return obj.toString();
            } else if (Integer.class.equals(hint) || int.class.equals(hint)) {
                return new Integer(((VTDouble)obj).intValue());
            } else if (Long.class.equals(hint) || long.class.equals(hint)) {
                return new Long(((VTDouble)obj).longValue());
            } else if (Boolean.class.equals(hint) || boolean.class.equals(hint)) {
                return new Boolean(((VTDouble)obj).booleanValue());
            }
        } else if(obj instanceof VTCollection) {
            return convertArray((VTCollection) obj, hint);
        } else if(obj instanceof VTStruct) {
            // TODO
        }
        
        return null;
    }

    /**
     * Convert an array into the desired object.
     *
     * @param arr.
     * @param hint.
     *
     * @return Object.
     */
    private Object convertArray(VTCollection arr, Class hint) {
        Object val = null;
        VTCollection fr = arr.getCollection(0);
        if(fr == null) {
            return null;
        }
        int width = fr.size();
        int height = arr.size();

        if (Integer.class.equals(hint) || int.class.equals(hint)) {
            val = new Integer(arr.getCollection(0).getLong(0).intValue());
        } else if (Double.class.equals(hint) || double.class.equals(hint)) {
            val = arr.getCollection(0).getDouble(0);
        } else if (String.class.equals(hint)) {
            val = arr.getCollection(0).getString(0);
        } else if (double[].class.equals(hint)) {
            double[] darr = new double[height * width];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getCollection(i / width).getDouble(i % width);
            }

            val = darr;
        } else if (double[][].class.equals(hint)) {
            double[][] darr = new double[height][width];

            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    darr[i][j] = arr.getCollection(i).getDouble(j);
                }
            }

            val = darr;
        } else if (Double[].class.equals(hint)) {
            Double[] darr = new Double[height * width];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getCollection(i / width).getDouble(i % width);
            }

            val = darr;
        } else if (Double[][].class.equals(hint)) {
            Double[][] darr = new Double[height][width];

            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    darr[i][j] = arr.getCollection(i).getDouble(j);
                }
            }

            val = darr;
        } else if (int[].class.equals(hint)) {
            int[] darr = new int[height * width];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getCollection(i / width).getLong(i % width).intValue();
            }

            val = darr;
        } else if (int[][].class.equals(hint)) {
            int[][] darr = new int[height][width];

            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    darr[i][j] = arr.getCollection(i).getLong(j).intValue();
                }
            }

            val = darr;
        } else if (Integer[].class.equals(hint)) {
            Integer[] darr = new Integer[height * width];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getCollection(i / width).getLong(i % width).intValue();
            }

            val = darr;
        } else if (Integer[][].class.equals(hint)) {
            Integer[][] darr = new Integer[height][width];

            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    darr[i][j] = arr.getCollection(i).getLong(j).intValue();
                }
            }

            val = darr;
        } else if (boolean[].class.equals(hint)) {
            boolean[] darr = new boolean[height * width];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getCollection(i / width).getLong(i % width).intValue() == 1;
            }

            val = darr;
        } else if (boolean[][].class.equals(hint)) {
            boolean[][] darr = new boolean[height][width];

            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    darr[i][j] = arr.getCollection(i).getLong(j).intValue() == 1;
                }
            }

            val = darr;
        } else if (Boolean[].class.equals(hint)) {
            Boolean[] darr = new Boolean[height * width];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = new Boolean(arr.getCollection(i / width).getLong(i % width).intValue() == 1);
            }

            val = darr;
        } else if (Boolean[][].class.equals(hint)) {
            Boolean[][] darr = new Boolean[height][width];

            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    darr[i][j] = new Boolean(arr.getCollection(i).getLong(j).intValue() == 1);
                }
            }

            val = darr;
        } else if (String[].class.equals(hint)) {
            String[] darr = new String[height * width];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = arr.getCollection(i / width).getString(i % width);
            }

            val = darr;
        } else if (String[][].class.equals(hint)) {
            String[][] darr = new String[height][width];

            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    darr[i][j] = arr.getCollection(i).getString(j);
                }
            }

            val = darr;
        } else if (Object[].class.equals(hint)) {
            Object[] darr = new Object[height * width];

            for (int i = 0; i < darr.length; i++) {
                darr[i] = createFrom(arr.getCollection(i / width).get(i % width), Object.class);
            }

            val = darr;
        } else if (Object[][].class.equals(hint)) {
            Object[][] darr = new Object[height][width];

            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    darr[i][j] = createFrom(arr.getCollection(i).get(j), Object.class);
                }
            }

            val = darr;
        } else {
            String str = arr.getCollection(0).getString(0);
            val = registry.get(str);
        }

        return val;
    }
    
    public Object[] convert(VTCollection args, Class[] hints) {
        Object[] a = new Object[hints.length];
        int csize = args.size();
        for(int i = 0; i < hints.length; i++) {
            if(i < csize) {
                a[i] = createFrom(args.get(i), hints[i]);
            } else {
                a[i] = null;
            }
        }
        return a;
    }
}
