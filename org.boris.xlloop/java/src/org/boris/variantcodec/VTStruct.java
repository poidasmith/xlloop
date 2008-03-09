package org.boris.variantcodec;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class VTStruct extends Variant {
    private Map<String, Variant> fields = new LinkedHashMap();

    public void clear() {
        fields.clear();
    }

    public int size() {
        return fields.size();
    }

    public Variant getValue(String name) {
        return fields.get(name);
    }

    public VTStruct getStruct(String name) {
        Variant dr = fields.get(name);
        if (dr instanceof VTStruct) {
            return (VTStruct) dr;
        } else {
            return null;
        }
    }

    public VTCollection getCollection(String name) {
        Variant dr = fields.get(name);
        if (dr instanceof VTCollection) {
            return (VTCollection) dr;
        } else {
            return null;
        }
    }

    public String getString(String name) {
        Variant dr = fields.get(name);
        if (dr instanceof VTString) {
            return ((VTString) dr).get();
        }
        return null;
    }

    public Double getDouble(String name) {
        Variant dr = fields.get(name);
        if (dr instanceof VTDouble) {
            return ((VTDouble) dr).doubleValue();
        } else if (dr instanceof VTLong) {
            return new Double(((VTLong) dr).longValue());
        } else {
            return null;
        }
    }

    public Long getLong(String name) {
        Variant dr = fields.get(name);
        if (dr instanceof VTLong) {
            return ((VTLong) dr).longValue();
        } else if (dr instanceof VTLong) {
            return new Long(((VTDouble) dr).longValue());
        } else {
            return null;
        }
    }

    public Integer getInteger(String name) {
        Long l = getLong(name);
        if (l != null) {
            return l.intValue();
        } else {
            return null;
        }
    }

    public boolean getBoolean(String name) {
        Long l = getLong(name);
        return l != null && l.intValue() == 1;
    }

    public Set<String> getKeys() {
        return Collections.unmodifiableSet(fields.keySet());
    }

    public void add(String name, Variant value) {
        fields.put(name, value);
    }

    public void add(String name, String value) {
        add(name, new VTString(value));
    }

    public void add(String name, Long value) {
        add(name, new VTLong(value));
    }

    public void add(String name, long value) {
        add(name, new VTLong(value));
    }

    public void add(String name, Double value) {
        add(name, new VTDouble(value));
    }

    public void add(String name, double value) {
        add(name, new VTDouble(value));
    }

    public void add(String name, int value) {
        add(name, new VTLong(value));
    }

    public void add(String name, boolean value) {
        add(name, new VTLong(value ? 1 : 0));
    }

    public String toString() {
        return Codec.encodeFormatted(this);
    }
}
