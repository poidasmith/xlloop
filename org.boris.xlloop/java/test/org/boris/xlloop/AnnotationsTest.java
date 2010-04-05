package org.boris.xlloop;

import java.io.File;

import org.boris.xlloop.reflect.XLFunction;

public class AnnotationsTest
{
    @XLFunction(name = "ListFiles", 
            help = "List the files contained within a directory", 
            args = { "dir" }, 
            argHelp = { "The directory" }, 
            category = "Files")
    public static String[] listFiles(String dir) {
        return new File(dir).list();
    }
}
