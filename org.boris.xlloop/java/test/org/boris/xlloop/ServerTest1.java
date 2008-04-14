package org.boris.xlloop;

import java.io.File;

import org.boris.xlloop.FunctionServer;
import org.boris.xlloop.reflect.ReflectFunctionHandler;
import org.boris.xlloop.script.LispFunctionHandler;
import org.boris.xlloop.script.ScriptRepository;
import org.boris.xlloop.util.CompositeFunctionHandler;
import org.boris.xlloop.util.CompositeRequestHandler;
import org.boris.xlloop.util.DebugFunctionHandler;
import org.boris.xlloop.util.DebugRequestHandler;
import org.boris.xlloop.util.FunctionInformationRequestHandler;

public class ServerTest1 {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer();
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        LispFunctionHandler lfh = new LispFunctionHandler();
        lfh.eval(new File("functions"), true); // evaluate any lisp files
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
