package org.boris.xlloop;

import java.io.File;

import org.boris.xlloop.script.ScriptRepository;
import org.boris.xlloop.util.XLList;
import org.boris.xlloop.xloper.XLoper;

public class ScriptRepTest1
{
    public static void main(String[] args) throws Exception {
        ScriptRepository rep = new ScriptRepository(new File(
                "F:\\eclipse\\workspace\\org.boris.functionserver\\functions"), "Script.");

        Thread.sleep(1000);

        XLList a = new XLList();
        a.add(4);
        a.add(4.5);
        XLoper res = rep.execute(null, "Script.sum", a.toArray());
        System.out.println(res);
    }
}
