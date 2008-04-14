package org.boris.xlloop.util;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;

public class IO
{
    public static String toString(Reader r) throws IOException {
        StringWriter sw = new StringWriter();
        copy(r, sw, true);
        return sw.toString();
    }

    public static String toString(File f) throws IOException {
        return toString(new FileReader(f));
    }

    public static void copy(Reader r, Writer w, boolean close) throws IOException {
        char[] buf = new char[4096];
        int len = 0;
        while ((len = r.read(buf)) > 0) {
            w.write(buf, 0, len);
        }
        if (close) {
            r.close();
            w.close();
        }
    }

    public static void copy(InputStream r, OutputStream w, boolean close) throws IOException {
        byte[] buf = new byte[4096];
        int len = 0;
        while ((len = r.read(buf)) > 0) {
            w.write(buf, 0, len);
        }
        if (close) {
            r.close();
            w.close();
        }
    }

    public static String getExtension(File f) {
        if (f == null)
            return null;
        String n = f.getName();
        int idx = n.lastIndexOf('.');
        if (idx == -1) {
            return null;
        }
        return n.substring(idx + 1);
    }
}
