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

public class XLOperType
{
    public static final int xltypeNum = 0x0001;
    public static final int xltypeStr = 0x0002;
    public static final int xltypeBool = 0x0004;
    public static final int xltypeRef = 0x0008;
    public static final int xltypeErr = 0x0010;
    public static final int xltypeFlow = 0x0020;
    public static final int xltypeMulti = 0x0040;
    public static final int xltypeMissing = 0x0080;
    public static final int xltypeNil = 0x0100;
    public static final int xltypeSRef = 0x0400;
    public static final int xltypeInt = 0x0800;
    public static final int xlbitXLFree = 0x1000;
    public static final int xlbitDLLFree = 0x4000;
}
