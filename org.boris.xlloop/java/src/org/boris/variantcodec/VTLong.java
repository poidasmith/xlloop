package org.boris.variantcodec;

public class VTLong extends Variant {
    private long value;

    public VTLong(long value) {
        this.value = value;
    }

    public long longValue() {
        return value;
    }

    public String toString() {
        return Long.toString(value);
    }
}
