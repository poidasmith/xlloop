/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.jxll;

import org.boris.jxll.util.LibraryLoader;

public class JNI
{
    private static final boolean DEBUG = true;
    private static final String VERSION = "0.0.2";
    private static final String BASE = "f:\\eclipse\\workspace\\org.boris.jxll\\jni\\build/";
    private static final String BUILD = "Debug";
    // private static final String BUILD = "Release";

    static {
        if (DEBUG) {
            System.load(BASE + "/XLLstub-" + "Debug" + "/xlcall32.dll");
            System.load(BASE + "/JXLL-" + BUILD + "/JXLL.dll");
        } else {
            LibraryLoader.load("xlcall32", JNI.class, true);
            LibraryLoader.load("jxll-" + VERSION, JNI.class, true);
        }
    }

    static native long loadLibrary(String filename);

    static native void dispose(Addin addin);

    static native void xlAutoOpen(long library);

    static native XLOper xlAddInManagerInfo(long library, XLOper action);

    static native XLOper invoke(long library, String function, int returnType, int[] argTypes, XLOper[] args);
}
