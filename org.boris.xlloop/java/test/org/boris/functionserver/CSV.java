package org.boris.functionserver;

import org.boris.variantcodec.VTStruct;

public class CSV {
    public static Object[][] toArray(String name) {
        Object[][] val = { 
                {name, "Hello"},
                {"This", "Row"}};
        return val;
    }

    public static Object[][] toArray(String name, boolean other) {
        Object[][] val = { 
                {name, new Boolean(other)},
                {"Method", "Overload"}};
        return val;
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
