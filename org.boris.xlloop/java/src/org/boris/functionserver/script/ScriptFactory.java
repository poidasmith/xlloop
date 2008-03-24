package org.boris.functionserver.script;

import java.io.File;
import java.io.IOException;

import org.apache.bsf.BSFException;
import org.apache.bsf.BSFManager;
import org.boris.functionserver.util.IO;

public class ScriptFactory {
    private static BSFManager manager = new BSFManager();
    
    public static Script create(File f) throws BSFException, IOException {
        String lang = BSFManager.getLangFromFilename(f.getName());
        String source = IO.toString(f);
        String name = f.getName();
        
        return new Script(manager, lang, source, name);
    }
    
}
