package org.boris.functionserver.script;

import java.io.IOException;
import java.io.Reader;

import org.boris.functionserver.Function;

public interface ScriptFactory 
{
    public Function create(Reader r) throws IOException;
}
