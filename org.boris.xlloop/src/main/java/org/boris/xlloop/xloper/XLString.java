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

public final class XLString extends XLoper
{
    public static final XLString EMPTY = new XLString("");
    
    public final String str;

    public XLString(String str) {
        super(xlTypeStr);
        this.str = trimToMaxLength(str);
    }

    private String trimToMaxLength(String str) {
        if (str == null)
            return "";
        if (str.length() > 255)
            return str.substring(0, 255);
        return str;
    }

    public String toString() {
        return "\"" + str + "\"";
    }
}
