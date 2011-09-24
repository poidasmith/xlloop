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
import org.boris.xlloop.xloper.XLNil;
import org.boris.xlloop.xloper.XLNum;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

public class CompTest1 implements IFunctionHandler
{
    private static String characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    public XLoper execute(IFunctionContext context, String name, XLoper[] args) throws RequestException {
        if (name.equals("RandTest")) {
            XLArray x = new XLArray((int) (Math.random() * 10 + 2), 1);
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
        } else if (name.equals("BasicTypes")) {
            XLList l = new XLList();
            l.add(34.5);
            l.add(65);
            l.add("hello");
            XLList p = new XLList();
            p.add(123.1213);
            p.add(234234.232);
            p.add(23.3333);
            l.add(p.toXLoper());
            return l.toXLoper();
        } else if (name.equals("Test222")) {
            XLList l = new XLList();
            l.add("m7EA7PaA3COwyuxoVk1YcVvANJjz0g");
            XLList n = new XLList();
            n.add(XLNil.NIL);
            n.add(XLNil.NIL);
            l.add(n.toXLoper());
            l.add(363.51324173151755);
            l.add(false);
            return l.toXLoper();

        } else if (name.equals("MakeRandomString")) {
            return new XLString(makeRandomString((int) ((XLNum) args[0]).num));
        } else if (name.equals("LongRunner")) {
            pause(8000);
            return new XLString("Finally...");
        } else if (name.equals("Pause")) {
            pause((long) ((XLNum) args[0]).num);
            return args[0];
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
            return new XLBool(Math.random() > 0.5);
        case 4:
            return new XLString(makeRandomString(0));
        case 5:
            return new XLString(makeRandomString(30, true));
        case 6:
            return new XLArray(2, 1);
        default:
            return XLMissing.MISSING;
        }
    }

    public static String makeRandomString(int len) {
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

    public static void pause(long ms) {
        try {
            Thread.sleep(ms);
        } catch (InterruptedException e) {
        }
    }
}
