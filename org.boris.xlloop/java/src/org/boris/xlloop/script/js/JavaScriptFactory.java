package org.boris.xlloop.script.js;

import java.io.IOException;
import java.io.Reader;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.Function;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.script.ScriptFactory;
import org.boris.xlloop.util.IO;

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
