package org.boris.variantcodec;

public class VTError extends Variant {
    private String str;

    public VTError(String str) {
        this.str = str;
    }

    public String get() {
        return str;
    }

    public int length() {
        return str.length();
    }

    public char charAt(int index) {
        return str.charAt(index);
    }

    public String toString() {
        return str;
    }
}
