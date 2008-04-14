package org.boris.xlloop;

import org.boris.variantcodec.VTStruct;

public class CSV {
    public static Object[][] toArray(String name) {
        Object[][] val = { { name, "Hello" }, { "This", "Row" } };
        return val;
    }

    public static Object[][] toArray(String name, boolean other) {
        Object[][] val = { { name, new Boolean(other) },
                { "Method", "Overload" } };
        return val;
    }
    
    public static double linint(double[] xn, double[] yn, double x) throws Exception {
        double res = 0;
        int xl = xn.length;

        if(xl == 0) {
            res = 0;
        } else if(xl == 1) {
            res = yn[0];
        } else if(x < xn[0]) {
            double dy = yn[1] - yn[0];
            double dx = xn[1] - xn[0];
            res = yn[0] - (xn[0] - x) * (dy/dx);
        } else if(x > xn[xl - 1]) {
            double dy = yn[xl - 1] - yn[xl - 2];
            double dx = xn[xl - 1] - xn[xl - 2];
            res = (x - xn[xl - 1]) * (dy/dx) + yn[xl - 1]; 
        } else {
            for(int i = 0; i < xl; i++) {
                if(i > 0 && xn[i] >= x) {
                    double dy = yn[i] - yn[i - 1];
                    double dx = xn[i] - xn[i - 1];
                    res = (x - xn[i - 1]) * (dy/dx) + yn[i - 1];
                    break;
                }
            }
        }

        return res;        
    }

    public static double sumV(double[] things) {
        double res = 0;
        for (int i = 0; i < things.length; i++) {
            res += things[i];
        }
        return res;
    }

    public static double sumM(double[][] things) {
        double res = 0;
        for (int i = 0; i < things.length; i++) {
            for (int j = 0; j < things[i].length; j++) {
                res += things[i][j];
            }
        }
        return res;
    }

    public static double sum(double a, double b) {
        return a + b;
    }

    public static double echo(double v) {
        return v;
    }

    public static VTStruct mapTest() {
        VTStruct s = new VTStruct();
        s.add("test", 2);
        s.add("hello", 3);
        return s;
    }
}
