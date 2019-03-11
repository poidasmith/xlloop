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

public class XLBool extends XLoper
{
    public static final XLBool TRUE = new XLBool(true);
    public static final XLBool FALSE = new XLBool(false);

    public final boolean bool;

    public XLBool(boolean bool) {
        super(xlTypeBool);
        this.bool = bool;
    }

    public String toString() {
        return Boolean.toString(bool);
    }
}
