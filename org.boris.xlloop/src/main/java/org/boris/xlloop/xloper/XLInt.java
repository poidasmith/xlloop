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

public class XLInt extends XLoper
{
    public final int w;

    public XLInt(int w) {
        super(xlTypeInt);
        this.w = w;
    }

    public String toString() {
        return Integer.toString(w);
    }
}
