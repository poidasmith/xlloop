package org.boris.functionserver.util;

import org.boris.functionserver.FunctionServer;
import org.boris.functionserver.reflect.ReflectFunctionHandler;
import org.boris.functionserver.reflect.ReflectRequestHandler;

public class ServerExample {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer();
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        rfh.addMethods("Math.", Math.class);
        DebugFunctionHandler dfh = new DebugFunctionHandler(rfh);
        fs.setFunctionHandler(dfh);
        fs.setRequestHandler(new ReflectRequestHandler(rfh));
        System.out.println("Listening on port 5454...");
        fs.run();
    }
}
