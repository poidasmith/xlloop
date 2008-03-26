package org.boris.functionserver.util;

import java.io.File;

import org.boris.functionserver.FunctionServer;
import org.boris.functionserver.reflect.ReflectFunctionHandler;
import org.boris.functionserver.script.ScriptRepository;

public class ServerExample {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer();
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        ScriptRepository srep = new ScriptRepository(new File("functions"),
                "Script.");
        rfh.addMethods("Math.", Math.class);
        CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        cfh.add(rfh);
        cfh.add(srep);
        fs.setFunctionHandler(cfh);
        fs.run();
    }
}
