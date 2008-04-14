package org.boris.xlloop.util;

import org.boris.xlloop.FunctionServer;
import org.boris.xlloop.reflect.ReflectFunctionHandler;

public class ServerExample {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer();
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        rfh.addMethods("Math.", Math.class);
        DebugFunctionHandler dfh = new DebugFunctionHandler(rfh);
        fs.setFunctionHandler(dfh);
        FunctionInformationRequestHandler firh = new FunctionInformationRequestHandler();
        firh.add(rfh.getFunctions());
        fs.setRequestHandler(new DebugRequestHandler(firh));
        System.out.println("Listening on port 5454...");
        fs.run();
    }
}
