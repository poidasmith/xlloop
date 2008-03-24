package org.boris.functionserver.util;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;

public class IO {

    public static String toString(File f) throws IOException {
        StringWriter sw = new StringWriter();
        copy(new FileReader(f), sw, true);
        return sw.toString();
    }

    public static void copy(Reader r, Writer w, boolean close) throws IOException {
        char[] buf = new char[4096];
        int len = 0;
        while((len = r.read(buf)) > 0) {
            w.write(buf, 0, len);
        }
        if(close) {
            r.close();
            w.close();
        }
    }
    
    public static void copy(InputStream r, OutputStream w, boolean close) throws IOException {
        byte[] buf = new byte[4096];
        int len = 0;
        while((len = r.read(buf)) > 0) {
            w.write(buf, 0, len);
        }
        if(close) {
            r.close();
            w.close();
        }
    }
}
