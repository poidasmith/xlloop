package org.boris.functionserver.script;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.bsf.BSFException;
import org.apache.bsf.BSFManager;
import org.boris.functionserver.Function;
import org.boris.functionserver.FunctionHandler;
import org.boris.functionserver.RequestException;
import org.boris.functionserver.util.FileSystemWatcher;
import org.boris.functionserver.util.IO;
import org.boris.functionserver.util.FileSystemWatcher.Callback;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;

public class ScriptRepository implements Callback, FunctionHandler {
    private File baseDir;
    private FileSystemWatcher watcher;
    private Map scripts = new HashMap();
    private Map factories = new HashMap();
    private String namespace;

    public ScriptRepository(File baseDir, String namespace) {
        this.baseDir = baseDir;
        this.watcher = new FileSystemWatcher(baseDir, this);
        this.namespace = namespace;
        watcher.start();
    }
    
    public void addFactory(String language, ScriptFactory factory) {
        factories.put(language, factory);
    }

    public void setWatcherPauseMillis(int millis) {
        watcher.setPauseMillis(millis);
    }

    public void stop() {
        watcher.shutdown();
    }

    public Function get(String name) {
        return (Function) scripts.get(name);
    }

    public void fileAdded(File f) {
        fileChanged(f);
    }

    public void fileChanged(File f) {
        String n = toName(f);
        if (n == null) {
            return;
        }
        System.out.println("Adding script: " + n);
        try {
            scripts.put(n, create(f));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void fileRemoved(File f) {
        scripts.remove(toName(f));
    }

    private String toName(File f) {
        String s = baseDir.toString();
        String fs = f.toString();
        fs = fs.substring(s.length() + 1);
        String ext = f.getName();
        int doti = ext.indexOf('.');

        if (doti != -1) {
            doti = ext.length() - doti;
            fs = fs.substring(0, fs.length() - doti);
        } else {
            return null; // We reject files without an extension.
        }

        fs = fs.replaceAll("\\\\", ".");
        if (namespace != null) {
            fs = namespace + fs;
        }

        return fs;
    }

    public Variant execute(String name, VTCollection args)
            throws RequestException {
        Function f = (Function) scripts.get(name);
        if (f == null) {
            throw new RequestException("#Unknown script: " + name);
        }
        return f.execute(args);
    }

    public boolean hasFunction(String name) {
        return scripts.containsKey(name);
    }
    
    public Function create(File f) throws BSFException, IOException {
        String lang = BSFManager.getLangFromFilename(f.getName());
        ScriptFactory factory = (ScriptFactory) factories.get(lang);
        if(factory != null) {
            return factory.create(new FileReader(f));
        } else {
            String source = IO.toString(f);
            String name = f.getName();
            
            return new BSFScript(lang, source, name);
        }
    }
}
