package org.boris.functionserver.script;

import java.io.File;

public class ScriptRepository {
    private File baseDir;

    public ScriptRepository(File baseDir) {
        this.baseDir = baseDir;
    }

    public Script get(String name) {
        return null;
    }

    public String[] getScriptNames() {
        return null;
    }
}
