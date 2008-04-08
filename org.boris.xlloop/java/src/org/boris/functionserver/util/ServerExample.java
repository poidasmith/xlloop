package org.boris.functionserver.util;

import org.boris.functionserver.FunctionServer;
import org.boris.functionserver.reflect.ReflectFunctionHandler;

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
