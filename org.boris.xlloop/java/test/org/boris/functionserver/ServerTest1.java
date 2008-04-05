package org.boris.functionserver;

import java.io.File;

import org.boris.functionserver.reflect.ReflectFunctionHandler;
import org.boris.functionserver.script.ScriptRepository;
import org.boris.functionserver.util.CompositeFunctionHandler;
import org.boris.functionserver.util.DebugFunctionHandler;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTStruct;
import org.boris.variantcodec.Variant;

public class ServerTest1 {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer();
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        ScriptRepository srep = new ScriptRepository(new File("functions"),
                "Script.");
        rfh.addMethods("Math.", Math.class);
        rfh.addMethods("CSV.", CSV.class);
        CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        cfh.add(rfh);
        cfh.add(srep);
        fs.setFunctionHandler(new DebugFunctionHandler(cfh));
        RequestMap rh = new RequestMap();
        rh.add("GetFunctions", new Request() {
            public Variant execute(VTStruct args) throws RequestException {
                return new VTCollection().add(new VTStruct().add("functionName", "Math.random"));
            }});
        fs.setRequestHandler(rh);
        fs.run();
    }
}
