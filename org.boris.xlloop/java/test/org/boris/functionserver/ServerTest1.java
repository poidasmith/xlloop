package org.boris.functionserver;

import java.io.File;

import org.boris.functionserver.reflect.ReflectFunctionHandler;
import org.boris.functionserver.script.LispFunctionHandler;
import org.boris.functionserver.script.ScriptRepository;
import org.boris.functionserver.util.CompositeFunctionHandler;
import org.boris.functionserver.util.CompositeRequestHandler;
import org.boris.functionserver.util.DebugFunctionHandler;
import org.boris.functionserver.util.DebugRequestHandler;
import org.boris.functionserver.util.FunctionInformationRequestHandler;

public class ServerTest1 {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer();
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        LispFunctionHandler lfh = new LispFunctionHandler();
        ScriptRepository srep = new ScriptRepository(new File("functions"),
                "Script.");
        rfh.addMethods("Math.", Math.class);
        rfh.addMethods("CSV.", CSV.class);
        rfh.addMethods("Math.", CompoundMathFunctions.class);
        CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        cfh.add(rfh);
        cfh.add(srep);
        cfh.add(lfh);
        CompositeRequestHandler crh = new CompositeRequestHandler();
        FunctionInformationRequestHandler firh = new FunctionInformationRequestHandler();
        firh.add(rfh.getFunctions());
        firh.add(lfh.getInformation());
        crh.add(firh);
        fs.setFunctionHandler(new DebugFunctionHandler(cfh));
        fs.setRequestHandler(new DebugRequestHandler(crh));
        fs.run();
    }
}
