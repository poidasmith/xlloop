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

public abstract class XLoper
{
    public static final int xlTypeNum = 0x1;
    public static final int xlTypeStr = 0x2;
    public static final int xlTypeBool = 0x4;
    public static final int xlTypeErr = 0x10;
    public static final int xlTypeMulti = 0x40;
    public static final int xlTypeMissing = 0x80;
    public static final int xlTypeNil = 0x100;
    public static final int xlTypeInt = 0x800;

    public final int type;

    XLoper(int type) {
        this.type = type;
    }
}
