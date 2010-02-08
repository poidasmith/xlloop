package org.boris.xlloop;

import java.io.File;

import org.boris.xlloop.handler.CompositeFunctionHandler;
import org.boris.xlloop.handler.DebugFunctionHandler;
import org.boris.xlloop.handler.FunctionInformationHandler;
import org.boris.xlloop.script.LispFunctionHandler;

public class LispTest1
{
    public static void main(String[] args) throws Exception {
        // Create a new function server on default port
        FunctionServer fs = new FunctionServer();

        // Create our lisp function handler
        LispFunctionHandler lfh = new LispFunctionHandler();

        // Evaluate any lisp files in this directory (and sub-dirs)
        lfh.eval(new File("functions"), true);

        // Expose a function called "Eval" for the lisp handler
        FunctionInformationHandler firh = new FunctionInformationHandler();
        firh.add(lfh.getInformation());

        // Create a composite function handler
        CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        cfh.add(firh);
        cfh.add(lfh);

        // Set the function handler
        fs.setFunctionHandler(new DebugFunctionHandler(cfh));

        // Run the engine
        fs.run();
    }
}
