package org.boris.functionserver;


public class ServerTest {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer(5600);
        fs.setFunctionHandler(new ExecuteHandler());
        fs.run();
    }
}
