package org.boris.functionserver;

import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class Request {
    private String type;
    private VTStruct args = new VTStruct();

    public Request(String type) {
        this.type = type;
    }

    public void addArg(String name, Variant value) {
        this.args.add(name, value);
    }

    public void addArg(String name, String value) {
        args.add(name, value);
    }

    public void addArg(String name, double value) {
        args.add(name, value);
    }

    public void addArg(String name, long value) {
        args.add(name, value);
    }

    public void addArg(String name, int value) {
        args.add(name, value);
    }

    public void addArg(String name, boolean value) {
        args.add(name, value);
    }

    public String getType() {
        return type;
    }

    public VTStruct getArgs() {
        return args;
    }

    public Variant encode() {
        VTStruct str = new VTStruct();
        str.add("type", type);
        str.add("args", args);
        return str;
    }
}
