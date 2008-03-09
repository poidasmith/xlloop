package org.boris.variantcodec;

import java.util.List;
import java.util.ArrayList;

public class VTCollection extends Variant {
    private List<Variant> items = new ArrayList();

    public void add(Variant dr) {
        items.add(dr);
    }

    public void add(String s) {
        add(new VTString(s));
    }

    public void add(long l) {
        add(new VTLong(l));
    }

    public void add(double d) {
        add(new VTDouble(d));
    }

    public void add(int i) {
        add(new VTLong(i));
    }

    public Variant get(int index) {
        return items.get(index);
    }

    public int size() {
        return items.size();
    }

    public void clear() {
        items.clear();
    }

    public String toString() {
        return Codec.encodeFormatted(this);
    }
}
