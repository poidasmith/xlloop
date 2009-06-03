package org.boris.xlloop;

import org.boris.xlloop.util.XLMap;
import org.boris.xlloop.xloper.XLArray;
import org.boris.xlloop.xloper.XLError;
import org.boris.xlloop.xloper.XLoper;

public class Test
{
    public static Object[][] toArray(String name) {
        Object[][] val = { { name, "Hello" }, { "This", "Row" } };
        return val;
    }

    public static Object[][] toArray(String name, boolean other) {
        Object[][] val = { { name, new Boolean(other) }, { "Method", "Overload" } };
        return val;
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

    public static XLoper enc() {
        XLMap m = new XLMap();
        m.add("boolt", true);
        m.add("boolf", false);
        m.add("i1", 2343);
        m.add("sdf", 34.2);
        m.add("asdf", 4545345.454345345);
        m.add("as33", XLError.NAME);

        return m.toXloper();
    }

    public static double echo(double v) {
        return v;
    }

    public static XLoper mapTest() {
        XLMap s = new XLMap();
        s.add("test", 2);
        s.add("hello", 3);
        return s.toXloper();
    }

    public static XLArray makeRandoms(int rows, int cols) {
        XLArray xa = new XLArray(rows, cols);
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                xa.set(i, j, Math.random());
            }
        }

        return xa;
    }

    public static String[] testStrings(double val) {
        return new String[] { "", "", "", "asdf" };
    }

    public static String[] testStrings2(double val) {
        return new String[] { " ", " ", " " };
    }

    public static String[] testStrings3(double val) {
        return new String[] { " ", null, null };
    }
}
