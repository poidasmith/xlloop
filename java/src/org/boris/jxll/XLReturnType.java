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

public class XLReturnType
{
    public static final int xlretSuccess = 0; /* success */
    public static final int xlretAbort = 1; /* macro halted */
    public static final int xlretInvXlfn = 2; /* invalid function number */
    public static final int xlretInvCount = 4; /* invalid number of arguments */
    public static final int xlretInvXloper = 8; /* invalid OPER structure */
    public static final int xlretStackOvfl = 16; /* stack overflow */
    public static final int xlretFailed = 32; /* command failed */
    public static final int xlretUncalced = 64; /* uncalced cell */
}
