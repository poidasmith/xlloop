package org.boris.xlloop;

import java.io.File;

import org.boris.xlloop.handler.DebugFunctionHandler;
import org.boris.xlloop.handler.FunctionInformationFunctionHandler;
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
        FunctionInformationFunctionHandler firh = new FunctionInformationFunctionHandler();
        firh.add(lfh.getInformation());

        // Set the function handler
        fs.setFunctionHandler(new DebugFunctionHandler(lfh));

        // Run the engine
        fs.run();
    }
}
