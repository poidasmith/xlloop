package org.boris.functionserver;

import java.io.FileReader;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Script;
import org.mozilla.javascript.Scriptable;

public class ScriptTest1 {
    public static void main(String[] args) throws Exception {
        Context cx = Context.enter();
        Scriptable scope = cx.initStandardObjects();
        Script sc = cx.compileReader(new FileReader("functions/sum.js"), "sum",
                1, null);
        sc.exec(cx, scope);
    }
}
