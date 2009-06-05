package org.boris.xlloop;

import java.io.File;

import org.boris.xlloop.handler.CompositeFunctionHandler;
import org.boris.xlloop.handler.DebugFunctionHandler;
import org.boris.xlloop.handler.FunctionInformationFunctionHandler;
import org.boris.xlloop.reflect.Reflect;
import org.boris.xlloop.reflect.ReflectFunctionHandler;
import org.boris.xlloop.script.LispFunctionHandler;
import org.boris.xlloop.script.ScriptRepository;
import org.boris.xlloop.util.CSV;
import org.boris.xlloop.util.Maths;

public class ServerTest1
{
    public static void main(String[] args) throws Exception {
        FunctionServer fs = createServer();
        fs.run();
    }

    public static FunctionServer createServer() {
        return new FunctionServer(5454, new DebugFunctionHandler(createHandler()));
    }

    public static FunctionHandler createHandler() {
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        LispFunctionHandler lfh = new LispFunctionHandler();
        lfh.eval(new File("functions"), true); // evaluate any lisp files
        ScriptRepository srep = new ScriptRepository(new File("functions"), "Script.");
        rfh.addMethods("Math.", Math.class);
        rfh.addMethods("Math.", Maths.class);
        rfh.addMethods("Reflect.", Reflect.class);
        rfh.addMethods("CSV.", CSV.class);
        rfh.addMethods("Test.", Test.class);
        rfh.addMethods("Fin.", Finance1.class);
        CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        cfh.add(rfh);
        cfh.add(srep);
        cfh.add(lfh);
        FunctionInformationFunctionHandler firh = new FunctionInformationFunctionHandler();
        firh.add(rfh.getFunctions());
        firh.add(lfh.getInformation());
        firh.add(srep); // add script repository as a function provider
        cfh.add(firh);
        cfh.add(new CompTest1());
        return cfh;
    }
}
