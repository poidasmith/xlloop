/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr.util;

public class Statistics
{
    public static int factorial(int value) {
        if (value <= 1)
            return 1;
        return value * factorial(value - 1);
    }

    public static double combin(int num, int cho) {
        return factorial(num) / (factorial(cho) * factorial(num - cho));
    }

    public static double binomDist(int x, int n, double p, boolean cumulative) {
        if (!cumulative) {
            return combin(n, x) * Math.pow(p, x) * Math.pow(1 - p, n - x);
        } else {
            double c = 0;
            for (int y = 0; y < x; y++) {
                c += binomDist(y, n, p, false);
            }
            return c;
        }
    }

    public static double critBinom(int n, double p, double alpha) {
        double c = 0;
        for (int y = 0; y < n; y++) {
            c += binomDist(y, n, p, false);
            if (c >= alpha)
                return y;
        }
        return 0;
    }

    public static double exponDist(double x, double lambda, boolean cumulative) {
        if (cumulative) {
            return (1 - Math.pow(Math.E, -lambda * x));
        } else {
            return (lambda * Math.pow(Math.E, -lambda * x));
        }
    }

    public static double confidence(double alpha, double stdev, int size) {
        return 1.96 * stdev / Math.sqrt(size);
    }
}
