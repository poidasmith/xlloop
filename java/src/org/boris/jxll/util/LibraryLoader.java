/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.jxll.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;

public class LibraryLoader
{
    private static String SEPARATOR;
    private static String TMP_PATH;
    static {
        SEPARATOR = System.getProperty("file.separator");
        TMP_PATH = System.getProperty("java.io.tmpdir");
        TMP_PATH = new File(TMP_PATH).getAbsolutePath();
    }

    public static boolean load(String libName, Class ref, boolean throwOnFailure) {
        if (load(libName, true))
            return true;
        if (load(TMP_PATH + SEPARATOR + libName, true))
            return true;
        if (extractAndLoad(ref, libName))
            return true;

        if (throwOnFailure)
            throw new UnsatisfiedLinkError("no " + libName +
                    " in java.library.path or on the classpath");
        else
            return false;
    }

    public static boolean extractAndLoad(Class ref, String libName) {
        try {
            InputStream is = ref.getResourceAsStream(libName + ".dll");
            if (is == null)
                return false;
            File file = new File(TMP_PATH + SEPARATOR + libName + ".dll");
            byte[] buffer = new byte[4096];
            FileOutputStream os = new FileOutputStream(file);
            int read;
            while ((read = is.read(buffer)) != -1) {
                os.write(buffer, 0, read);
            }
            os.close();
            is.close();
            if (load(file.getAbsolutePath(), false))
                return true;
        } catch (Throwable t) {
        }

        return false;
    }

    public static boolean load(String libName, boolean appendExtension) {
        try {
            if (libName.indexOf(SEPARATOR) != -1) {
                String ln = libName;
                if (appendExtension)
                    ln += ".dll";
                System.load(ln);
            } else {
                System.loadLibrary(libName);
            }
            return true;
        } catch (UnsatisfiedLinkError e) {
        }
        return false;
    }
}
