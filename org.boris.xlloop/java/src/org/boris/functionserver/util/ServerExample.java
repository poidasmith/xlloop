package org.boris.functionserver.util;

import org.boris.functionserver.FunctionServer;
import org.boris.functionserver.reflect.ReflectFunctionHandler;

public class ServerExample {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer();
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        rfh.addMethods("Math.", Math.class);
        CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        cfh.add(rfh);
        fs.setFunctionHandler(cfh);
        fs.run();
    }
}
