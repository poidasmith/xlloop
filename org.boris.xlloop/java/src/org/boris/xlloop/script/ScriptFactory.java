package org.boris.xlloop.script;

import java.io.IOException;
import java.io.Reader;

import org.boris.xlloop.Function;

public interface ScriptFactory 
{
    public Function create(Reader r) throws IOException;
}
