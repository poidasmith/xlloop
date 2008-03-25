package org.boris.functionserver;

import java.io.FileReader;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Script;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

public class ScriptTest1 {
    public static void main(String[] args) throws Exception {
        //VTCollection input = new VTCollection();
        Object[] input = {new Integer(4), new Integer(5)};
        Context cx = Context.enter();
        Scriptable scope = cx.initStandardObjects();
        Script sc = cx.compileReader(new FileReader("functions/sum.js"), "sum",
                1, null);
        ScriptableObject.putProperty(scope, "args", Context.javaToJS(input, scope));
        Object o = sc.exec(cx, scope);
        System.out.println(o);
    }
}
