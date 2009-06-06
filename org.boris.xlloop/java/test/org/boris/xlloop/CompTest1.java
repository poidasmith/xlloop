/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop;

import org.boris.xlloop.util.XLList;
import org.boris.xlloop.xloper.XLArray;
import org.boris.xlloop.xloper.XLBool;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLMissing;
import org.boris.xlloop.xloper.XLNum;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

public class CompTest1 implements FunctionHandler
{
    private static String characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    public XLoper execute(String name, XLoper[] args) throws RequestException {
        if (name.equals("RandTest")) {
            XLArray x = new XLArray((int) (Math.random() * 50 + 2), 1);
            for (int i = 0; i < x.length; i++) {
                x.array[i] = makeRandom();
            }
            return x;
        } else if (name.equals("ArrayTest")) {
            // Threads.sleep(15000);
            XLArray x = new XLArray(15, 10);
            for (int i = 0; i < 15; i++) {
                int len = (int) (Math.random() * 10) + 1;
                for (int j = 0; j < len; j++) {
                    x.set(i, j, i * j);
                }
            }
            return x;
        } else if (name.equals("ArgsTest")) {
            return new XLList(args).toXLoper();
        }
        return new XLString("#Unknown Function");
    }

    public boolean hasFunction(String name) {
        return true;
    }

    public static XLoper makeRandom() {
        int choice = (int) (Math.random() * 7);
        switch (choice) {
        case 0:
            return new XLString(makeRandomString());
        case 1:
            return new XLNum(Math.random() * 1000);
        case 2:
            return new XLInt((int) (Math.random() * 1000));
        case 3:
            return new XLBool(Math.random() > 0.5 ? true : false);
        case 4:
            return new XLString(makeRandomString(0));
        case 5:
            return new XLString(makeRandomString(30, true));
        case 6:
            return new XLArray(2, 1000);
        default:
            return XLMissing.MISSING;
        }
    }

    private static String makeRandomString(int len) {
        char[] c = new char[len];
        int cl = characters.length();
        for (int i = 0; i < len; i++) {
            c[i] = characters.charAt((int) (Math.random() * cl));
        }
        return new String(c);
    }

    private static String makeRandomString(int len, boolean unicode) {
        if (!unicode)
            return makeRandomString(len);
        char[] c = new char[len];
        for (int i = 0; i < len; i++) {
            c[i] = (char) (characters.charAt((int) (Math.random() * characters.length())));
        }
        return new String(c);
    }

    private static String makeRandomString() {
        int len = (int) (Math.random() * 260);
        return makeRandomString(len);
    }
}
