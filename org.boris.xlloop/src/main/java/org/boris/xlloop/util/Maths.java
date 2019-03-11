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

public class Maths
{
    public static double linearInterp(double[] xn, double[] yn, double x) {
        double res = 0;
        int xl = xn.length;

        if (xl == 0) {
            res = 0;
        } else if (xl == 1) {
            res = yn[0];
        } else if (x < xn[0]) {
            double dy = yn[1] - yn[0];
            double dx = xn[1] - xn[0];
            res = yn[0] - (xn[0] - x) * (dy / dx);
        } else if (x > xn[xl - 1]) {
            double dy = yn[xl - 1] - yn[xl - 2];
            double dx = xn[xl - 1] - xn[xl - 2];
            res = (x - xn[xl - 1]) * (dy / dx) + yn[xl - 1];
        } else {
            for (int i = 0; i < xl; i++) {
                if (i > 0 && xn[i] >= x) {
                    double dy = yn[i] - yn[i - 1];
                    double dx = xn[i] - xn[i - 1];
                    res = (x - xn[i - 1]) * (dy / dx) + yn[i - 1];
                    break;
                }
            }
        }

        return res;
    }

    public static double[] normalDist(int numRows) {
        double[] items = new double[numRows];
        for (int i = 0; i < numRows; i++) {
            items[i] = Math.random() + Math.random() + Math.random() +
                    Math.random() + Math.random() + Math.random() - 3;
        }
        return items;
    }
}
