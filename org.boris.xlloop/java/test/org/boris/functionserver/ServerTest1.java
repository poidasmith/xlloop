package org.boris.functionserver;

import java.io.File;

import org.boris.functionserver.reflect.ReflectFunctionHandler;
import org.boris.functionserver.reflect.ReflectRequestHandler;
import org.boris.functionserver.script.ScriptRepository;
import org.boris.functionserver.util.CompositeFunctionHandler;
import org.boris.functionserver.util.DebugFunctionHandler;
import org.boris.functionserver.util.FunctionInformation;
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
                FunctionInformation fi = new FunctionInformation("Math.pow");
                fi.setFunctionHelp("Raises the first value to the power of the second");
                fi.setCategory("Maths");
                fi.addArgument("value", "The first value");
                fi.addArgument("power", "The power value");
                System.out.println(fi.encode());
                return new VTCollection().add(fi.encode());
            }});
        fs.setRequestHandler(new ReflectRequestHandler(rfh));
        //fs.setRequestHandler(rh);
        fs.run();
    }
}
