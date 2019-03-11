/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.xloper;

public class XLError extends XLoper
{
    public static final XLError NULL = new XLError(0);
    public static final XLError DIV0 = new XLError(7);
    public static final XLError VALUE = new XLError(15);
    public static final XLError REF = new XLError(23);
    public static final XLError NAME = new XLError(29);
    public static final XLError NUM = new XLError(36);
    public static final XLError NA = new XLError(42);

    public final int err;

    public XLError(int err) {
        super(xlTypeErr);
        this.err = err;
    }

    public String toString() {
        return Integer.toString(err);
    }
}
