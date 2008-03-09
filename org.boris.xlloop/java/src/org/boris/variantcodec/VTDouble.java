package org.boris.variantcodec;

public class VTDouble extends Variant {
    private double value;

    public VTDouble(double value) {
        this.value = value;
    }

    public double doubleValue() {
        return value;
    }

    public long longValue() {
        return Double.valueOf(value).longValue();
    }

    public int intValue() {
        return Double.valueOf(value).intValue();
    }

    public String toString() {
        return Double.toString(value);
    }
}
