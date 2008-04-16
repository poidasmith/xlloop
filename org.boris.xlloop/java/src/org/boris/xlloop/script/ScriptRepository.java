/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.script;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.bsf.BSFException;
import org.apache.bsf.BSFManager;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.Function;
import org.boris.xlloop.FunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.util.FileSystemWatcher;
import org.boris.xlloop.util.IO;
import org.boris.xlloop.util.FileSystemWatcher.Callback;

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
        
        // Add our specific factories
        JavaScriptFactory jsf = new JavaScriptFactory();
        factories.put("javascript", jsf);
        factories.put("js", jsf);
        
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
        try {
            Function fn = create(f);
            if(fn != null) {
                System.out.println("Adding script: " + n);
                scripts.put(n, fn);
            } else {
                System.out.println("Unrecognized: " + f.getName());
            }
        } catch (Exception e) {
            System.err.println("Error processing: " + f);
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
        String lang = null; 
        try {
            lang = BSFManager.getLangFromFilename(f.getName());
        } catch (Exception e) {
        }
        boolean bsf = lang != null;
        if(lang == null) {
            lang = IO.getExtension(f);
        }
        ScriptFactory factory = (ScriptFactory) factories.get(lang);
        if(factory != null) {
            return factory.create(new FileReader(f));
        } else if(bsf) {
            String source = IO.toString(f);
            String name = f.getName();
            
            return new BSFScript(lang, source, name);
        }
        
        return null;
    }
}
