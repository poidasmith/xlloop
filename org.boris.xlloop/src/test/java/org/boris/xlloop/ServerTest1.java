package org.boris.xlloop;

import java.io.File;

import org.boris.xlloop.handler.CompositeFunctionHandler;
import org.boris.xlloop.handler.DebugFunctionHandler;
import org.boris.xlloop.handler.FunctionInformationHandler;
import org.boris.xlloop.menu.MenuHandler;
import org.boris.xlloop.reflect.Reflect;
import org.boris.xlloop.reflect.ReflectFunctionHandler;
import org.boris.xlloop.util.CSV;
import org.boris.xlloop.util.Maths;

public class ServerTest1
{
    public static void main(String[] args) throws Exception {
        FunctionServer fs = createServer(5454);
        fs.run();
    }

    public static FunctionServer createServer(int port) {
        return new FunctionServer(port, new DebugFunctionHandler(createHandler()));
    }

    public static IFunctionHandler createHandler() {
        ReflectFunctionHandler rfh = new ReflectFunctionHandler();
        rfh.addMethods("Math.", Math.class);
        rfh.addMethods("Math.", Maths.class);
        rfh.addMethods("Reflect.", Reflect.class);
        rfh.addMethods("CSV.", CSV.class);
        rfh.addMethods("Test.", Test.class);
        rfh.addMethods("Fin.", Finance1.class);
        rfh.addMethods("", AnnotationsTest.class);
        CompositeFunctionHandler cfh = new CompositeFunctionHandler();
        cfh.add(rfh);
        FunctionInformationHandler firh = new FunctionInformationHandler();
        firh.add(rfh.getFunctions());
        cfh.add(firh);
        cfh.add(new MenuHandler(new TestMenu()));
        cfh.add(new CompTest1());
        return cfh;
    }
}
