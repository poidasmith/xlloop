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

public class XLMissing extends XLoper
{
    public static final XLMissing MISSING = new XLMissing();

    XLMissing() {
        super(xlTypeMissing);
    }

    public String toString() {
        return "";
    }
}
