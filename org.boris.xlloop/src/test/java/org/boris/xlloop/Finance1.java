package org.boris.xlloop;

public class Finance1
{
    public static double SimpleInterest(double capital, double rate, double years) {
        return capital * (1 + rate * years);
    }
    
    public static double CompoundInterest(double capital, double rate, double years) {
        return capital * Math.pow(1 + rate, years);
    }
    
    public static double ContinuousCompound(double capital, double rate, double years) {
        return capital * Math.pow(Math.E, rate * years);
    }
    
    public static double AnnuityPV(double payment, double rate, int periods) {
        double val = 0;
        for(int i = 1; i <= periods; i++) {
            val += payment / Math.pow(1 + rate, i); 
        }
        return val;
    }
    
    public static double AnnuityFV(double payment, double rate, int periods) {
        double val = 0;
        for(int i = 0; i <= periods; i++) {
            val += Math.pow(1 + rate, i); 
        }
        return payment * val;
    }
}
