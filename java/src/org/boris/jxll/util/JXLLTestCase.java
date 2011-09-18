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

import org.boris.jxll.Addin;
import org.boris.jxll.JXLL;

public class JXLLTestCase // extends TestCase
{
    protected Addin addin;

    protected void loadAddin(String filename) {
        loadAddin(new File(filename));
    }

    protected void loadAddin(File file) {
        addin = JXLL.load(file.toString());
    }

    protected void unloadAddin() {
        JXLL.dispose(addin);
    }
}
