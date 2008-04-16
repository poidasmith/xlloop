package org.boris.xlloop;

import java.io.File;

import org.boris.xlloop.handler.CompositeFunctionHandler;
import org.boris.xlloop.handler.CompositeRequestHandler;
import org.boris.xlloop.handler.DebugFunctionHandler;
import org.boris.xlloop.handler.DebugRequestHandler;
import org.boris.xlloop.handler.FunctionInformationRequestHandler;
import org.boris.xlloop.reflect.Reflect;
import org.boris.xlloop.reflect.ReflectFunctionHandler;
import org.boris.xlloop.script.LispFunctionHandler;
import org.boris.xlloop.script.ScriptRepository;
import org.boris.xlloop.util.CSV;

public class ServerTest1 {
    public static void main(String[] args) throws Exception {
        FunctionServer fs = new FunctionServer();
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        LispFunctionHandler lfh = new LispFunctionHandler();
        lfh.eval(new File("functions"), true); // evaluate any lisp files
        ScriptRepository srep = new ScriptRepository(new File("functions"),
                "Script.");
        rfh.addMethods("Math.", Math.class);
        rfh.addMethods("Reflect.", Reflect.class);
        rfh.addMethods("CSV.", CSV.class);
        rfh.addMethods("Test.", Test.class);
        rfh.addMethods("Math.", CompoundMathFunctions.class);
        CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        cfh.add(rfh);
        cfh.add(srep);
        cfh.add(lfh);
        CompositeRequestHandler crh = new CompositeRequestHandler();
        FunctionInformationRequestHandler firh = new FunctionInformationRequestHandler();
        firh.add(rfh.getFunctions());
        firh.add(lfh.getInformation());
        firh.add(srep); // add script repository as a function provider
        crh.add(firh);
        fs.setFunctionHandler(new DebugFunctionHandler(cfh));
        fs.setRequestHandler(new DebugRequestHandler(crh));
        fs.run();
    }
}
