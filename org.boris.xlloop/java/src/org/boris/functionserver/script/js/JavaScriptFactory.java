package org.boris.functionserver.script.js;

import java.io.IOException;
import java.io.Reader;

import org.boris.functionserver.Function;
import org.boris.functionserver.RequestException;
import org.boris.functionserver.script.ScriptFactory;
import org.boris.functionserver.util.IO;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;

public class JavaScriptFactory implements ScriptFactory
{
    public Function create(Reader r) throws IOException {
        String script = IO.toString(r);
        
        return null;
    }
    
    public static class JavaScriptFunction implements Function {

        public Variant execute(VTCollection args) throws RequestException {
            return null;
        }
    }
}
