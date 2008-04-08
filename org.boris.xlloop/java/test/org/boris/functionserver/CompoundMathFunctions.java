package org.boris.functionserver;

public class CompoundMathFunctions {
    public static double[] normalDist(int numRows) {
        double[] items = new double[numRows];
        for (int i = 0; i < numRows; i++) {
            items[i] = Math.random() + Math.random() + Math.random() + Math.random()
                    + Math.random() + Math.random() - 3;
        }
        return items;
    }
}
