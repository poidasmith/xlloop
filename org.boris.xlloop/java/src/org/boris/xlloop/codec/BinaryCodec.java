/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.codec;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.boris.xlloop.xloper.XLArray;
import org.boris.xlloop.xloper.XLBool;
import org.boris.xlloop.xloper.XLError;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLMissing;
import org.boris.xlloop.xloper.XLNil;
import org.boris.xlloop.xloper.XLNum;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;

public class BinaryCodec
{
    public static XLoper decode(InputStream is) throws IOException {
        int type = (int) readDoubleWord(is);
        switch (type) {
        case XLoper.xlTypeBool:
            return decodeBool(is);
        case XLoper.xlTypeErr:
            return decodeError(is);
        case XLoper.xlTypeInt:
            return decodeInt(is);
        case XLoper.xlTypeMissing:
            return decodeMissing();
        case XLoper.xlTypeMulti:
            return decodeMulti(is);
        case XLoper.xlTypeNil:
            return decodeNil();
        case XLoper.xlTypeNum:
            return decodeNum(is);
        case XLoper.xlTypeStr:
            return decodeStr(is);
        case -1:
            throw new IOException("EOF encountered");
        default:
            throw new IOException("Invalid type encountered: " + type);
        }
    }

    private static XLoper decodeStr(InputStream is) throws IOException {
        int len = is.read();
        byte[] b = new byte[len];
        is.read(b);
        return new XLString(new String(b));
    }

    private static XLoper decodeNum(InputStream is) throws IOException {
        return new XLNum(Double
                .longBitsToDouble(((long) readDoubleWord(is) << 32) |
                        (long) readDoubleWord(is)));
    }

    private static XLoper decodeNil() {
        return XLNil.NIL;
    }

    private static XLoper decodeMulti(InputStream is) throws IOException {
        int rows = (int) readDoubleWord(is);
        int cols = (int) readDoubleWord(is);
        int len = rows * cols;
        XLoper[] array = new XLoper[len];
        for (int i = 0; i < len; i++) {
            array[i] = decode(is);
        }

        return new XLArray(array, rows, cols);
    }

    private static XLoper decodeMissing() {
        return XLMissing.MISSING;
    }

    private static XLoper decodeInt(InputStream is) throws IOException {
        return new XLInt((int) readDoubleWord(is));
    }

    private static XLoper decodeError(InputStream is) throws IOException {
        long err = readDoubleWord(is);
        return new XLError((int) err);
    }

    private static XLoper decodeBool(InputStream is) throws IOException {
        int val = is.read();
        return new XLBool(val != 0);
    }

    public static void encode(XLoper xloper, OutputStream os)
            throws IOException {
        writeDoubleWord(xloper.type, os);
        switch (xloper.type) {
        case XLoper.xlTypeBool:
            encodeBoolean((XLBool) xloper, os);
            break;
        case XLoper.xlTypeErr:
            encodeError((XLError) xloper, os);
            break;
        case XLoper.xlTypeInt:
            encodeInteger((XLInt) xloper, os);
            break;
        case XLoper.xlTypeMissing:
            break;
        case XLoper.xlTypeMulti:
            encodeArray((XLArray) xloper, os);
            break;
        case XLoper.xlTypeNil:
            break;
        case XLoper.xlTypeNum:
            encodeDecimal((XLNum) xloper, os);
            break;
        case XLoper.xlTypeStr:
            encodeString((XLString) xloper, os);
            break;
        }
    }

    private static void encodeString(XLString xloper, OutputStream os)
            throws IOException {
        String str = xloper.str;
        if (str.length() > 255)
            str = str.substring(0, 255);
        os.write(str.length());
        os.write(str.getBytes());
    }

    private static void encodeDecimal(XLNum xloper, OutputStream os)
            throws IOException {
        long l = Double.doubleToLongBits(xloper.num);
        writeDoubleWord((int) (l >> 32), os);
        writeDoubleWord((int) l, os);
    }

    private static void encodeArray(XLArray xloper, OutputStream os)
            throws IOException {
        writeDoubleWord(xloper.rows, os);
        writeDoubleWord(xloper.columns, os);
        for (int i = 0; i < xloper.length; i++) {
            encode(xloper.array[i], os);
        }
    }

    private static void encodeInteger(XLInt xloper, OutputStream os)
            throws IOException {
        writeDoubleWord(xloper.w, os);
    }

    private static void encodeError(XLError xloper, OutputStream os)
            throws IOException {
        writeDoubleWord(xloper.err, os);
    }

    private static void encodeBoolean(XLBool xloper, OutputStream os)
            throws IOException {
        os.write(xloper.bool ? 1 : 0);
    }

    private static void writeDoubleWord(long v, OutputStream w)
            throws IOException {
        w.write((int) (v >> 24 & 0xff));
        w.write((int) (v >> 16 & 0xff));
        w.write((int) (v >> 8 & 0xff));
        w.write((int) (v & 0xff));
    }

    private static long readDoubleWord(InputStream is) throws IOException {
        return ((long) is.read() << 24) | ((long) is.read() << 16) |
                ((long) is.read() << 8) | (long) is.read();
    }
}
