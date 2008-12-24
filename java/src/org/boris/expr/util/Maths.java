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

public class Maths
{
    public static double round(double value, int dps) {
        boolean n = value < 0;
        if (n)
            value *= -1;
        double r = ((double) Math.round(value * Math.pow(10, dps))) /
                Math.pow(10, dps);
        if (n)
            r *= -1;
        return r;
    }

    public static double roundDown(double value, int dps) {
        boolean n = value < 0;
        if (n)
            value *= -1;
        double r = ((double) Math.floor(value * Math.pow(10, dps))) /
                Math.pow(10, dps);
        if (n)
            r *= -1;
        return r;
    }

    public static double roundUp(double value, int dps) {
        boolean n = value < 0;
        if (n)
            value *= -1;
        double r = ((double) Math.ceil(value * Math.pow(10, dps))) /
                Math.pow(10, dps);
        if (n)
            r *= -1;
        return r;
    }
}
