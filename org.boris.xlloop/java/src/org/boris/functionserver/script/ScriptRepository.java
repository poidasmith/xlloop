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

    public ScriptHelp getHelp(String name) {
        return null;
    }

    public EditableScript checkout(String name) {
        return null;
    }

    public void commit(String name, EditableScript script) {
    }
}
