/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import org.boris.xlloop.codec.BinaryCodec;
import org.boris.xlloop.xloper.XLoper;

public class XLoperCache
{
    private File baseDir;

    public XLoperCache(File baseDir) {
        this.baseDir = baseDir;
    }

    public String toId(Object... ids) {
        StringBuilder sb = new StringBuilder();
        for (Object o : ids) {
            sb.append(File.separatorChar);
            sb.append(cleanFile(String.valueOf(o)));
        }
        return sb.toString();
    }

    public XLoper get(File f) throws IOException {
        try {
            return BinaryCodec.decode(new BufferedInputStream(new FileInputStream(f)));
        } catch (Exception e) {
            f.delete();
            return null;
        }
    }

    public void put(XLoper x, File f) throws IOException {
        BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(f));
        BinaryCodec.encode(x, bos);
        bos.flush();
        bos.close();
    }

    public void put(XLoper x, String id) throws IOException {
        put(x, toFile(id));
    }

    public void put(XLoper x, Object... ids) throws IOException {
        put(x, toFile(toId(ids)));
    }

    public XLoper get(long date, Object... ids) throws IOException {
        File f = toFile(toId(ids));
        if (f != null && f.exists() && f.lastModified() > date)
            return get(f);
        return null;
    }

    public XLoper get(String id) throws IOException {
        return get(toFile(id));
    }

    public boolean isValid(long date, Object... ids) {
        return isValid(date, toId(ids));
    }

    public boolean isValid(long date, String id) {
        File f = toFile(id);
        return f != null && f.exists() && f.isFile() && f.lastModified() > date;
    }

    private File toFile(String id) {
        if (id == null)
            return null;
        id = id.replaceAll("[:\"*?<>|]+", "_");
        if (isNullOrEmpty(id))
            id = "default";
        if (!id.endsWith(".dat"))
            id += ".dat";
        File f = new File(baseDir, id);
        if (!f.getParentFile().exists())
            f.getParentFile().mkdirs();
        return f;
    }

    private static String cleanFile(String filename) {
        return filename.replaceAll("[\\\\\\\\/]+", "_");
    }

    private static boolean isNullOrEmpty(String str) {
        return str == null || str.trim().length() == 0;
    }
}
