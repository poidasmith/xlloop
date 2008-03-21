package org.boris.functionserver.xlobject;

public abstract class XLObject {
    public double asDouble() {
        return 0.0;
    }

    public int asInt() {
        return 0;
    }

    public String asString() {
        return null;
    }

    public int rows() {
        return 0;
    }

    public int columns() {
        return 0;
    }

    @SuppressWarnings("unused")
    public XLObject get(int row, int column) {
        return null;
    }
    
    public boolean isMissing() {
        return false;
    }
    
    public boolean isError() {
        return false;
    }
}
