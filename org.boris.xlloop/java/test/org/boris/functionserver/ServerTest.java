package org.boris.functionserver;

import org.boris.functionserver.handler.ExecuteHandler;

public class ServerTest {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer(5454);
        fs.addRequestHandler("Exec", new ExecuteHandler());
        fs.run();
    }
}
