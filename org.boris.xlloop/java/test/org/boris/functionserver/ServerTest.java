package org.boris.functionserver;

import org.boris.functionserver.reflect.ReflectFunctionHandler;


public class ServerTest {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer(5600);
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        rfh.addMethods("CSV.", CSV.class);
        rfh.addMethods("Math.", Math.class);
        fs.setFunctionHandler(rfh);
        fs.run();
    }
}
