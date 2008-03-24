package org.boris.functionserver.script;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.WeakHashMap;

import org.apache.bsf.BSFException;
import org.apache.bsf.BSFManager;
import org.boris.functionserver.util.IO;

public class ScriptFactory {
    private static Map<Thread, BSFManager> managers = new WeakHashMap(); 
    
    public static Script create(File f) throws BSFException, IOException {
        String lang = BSFManager.getLangFromFilename(f.getName());
        String source = IO.toString(f);
        String name = f.getName();
        
        return new Script(lang, source, name);
    }
    
    public static BSFManager getManager() {
        BSFManager m = managers.get(Thread.currentThread());
        if(m == null) {
            managers.put(Thread.currentThread(), m = new BSFManager());
        }
        return m;
    }
    
}
