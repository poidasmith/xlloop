package org.boris.xlloop;

import java.io.File;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.script.ScriptRepository;

public class ScriptRepTest1 {
    public static void main(String[] args) throws Exception {
        ScriptRepository rep = new ScriptRepository(new File(
        "F:\\eclipse\\workspace\\org.boris.functionserver\\functions"),
        "Script.");
        
        Thread.sleep(1000);
        
        VTCollection a = new VTCollection();
        a.add(4);
        a.add(4.5);
        Variant res = rep.execute("Script.sum", a);
        System.out.println(res);
        
    }
}
