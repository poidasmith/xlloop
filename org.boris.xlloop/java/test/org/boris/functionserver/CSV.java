package org.boris.functionserver;

public class CSV {
    public static Object[][] toArray(String name) {
        Object[][] val = { 
                {name, "Hello"},
                {"This", "Row"}};
        return val;
    }

    public static Object[][] toArray(String name, boolean other) {
        Object[][] val = { 
                {name, other},
                {"Method", "Overload"}};
        return val;
    }
}
