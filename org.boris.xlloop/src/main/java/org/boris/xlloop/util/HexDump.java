/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 *
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.util;

import sun.plugin2.main.client.WDonatePrivilege;

import java.io.IOException;
import java.io.InputStream;

public class HexDump {
    private static final int WIDTH = 20;

    public static void dump(byte[] b, int off, int len, Appendable w) throws IOException {
        int numRows = len / WIDTH;
        for (int i = 0; i < numRows; i++) {
            dumpRow(b, off + i * WIDTH, WIDTH, w);
            w.append("\n");
        }
        int leftover = len % WIDTH;
        if (leftover > 0) {
            dumpRow(b, off + b.length - leftover, leftover, w);
        }
    }

    public static void dump(byte[] data, Appendable w) throws IOException {
        dump(data, 0, data.length, w);
    }

    private static void dumpRow(byte[] data, int start, int length, Appendable w) throws IOException {
        for (int i = 0; i < length; i++) {
            String s = Integer.toHexString(data[start + i] & 0x00ff);
            if (s.length() == 1) {
                w.append("0");
            }
            w.append(s);
            w.append(" ");
        }
        if (length < WIDTH) {
            for (int i = 0; i < WIDTH - length; i++) {
                w.append("   ");
            }
        }
        for (int i = 0; i < length; i++) {
            byte b = data[start + i];
            if (Character.isLetterOrDigit(b)) {
                w.append((char) b);
            } else {
                w.append(".");
            }
        }
    }

    /**
     * Stream out incremental hex dumps
     */
    public static class HexDumpWriter {
        private final Appendable w;

        private int offset;

        public HexDumpWriter(Appendable w) {
            this.w = w;
        }

        public void dump(byte[] b) throws IOException {
            dump(b, 0, b.length);
        }

        public void dump(byte[] b, int off, int len) throws IOException {
            int trailing = Math.min(WIDTH - offset, len);
            HexDump.dump(b, off, trailing, w);
            if(len + offset >= WIDTH)
                w.append("\n");
            int remaining = len - trailing;
            if(remaining > 0) {
                HexDump.dump(b, off + trailing, remaining, w);
                offset = (offset + remaining) % WIDTH;
            }
        }
    }

    /**
     * Incrementally dumps the input stream as hex. Useful for debugging a stream of bytes.
     */
    public static class HexDumpInputStream extends InputStream {
        private final InputStream delegate;
        private final HexDumpWriter hex;
        private final byte[] temp = {};

        public HexDumpInputStream(InputStream delegate, Appendable hex) {
            this.delegate = delegate;
            this.hex = new HexDumpWriter(hex);
        }

        @Override
        public int read(byte[] b, int off, int len) throws IOException {
            int res = delegate.read(b, off, len);
            hex.dump(b, off, res);
            return res;
        }

        @Override
        public int read(byte[] b) throws IOException {
            int res = delegate.read(b);
            hex.dump(b, 0, res);
            return res;
        }

        @Override
        public int read() throws IOException {
            int res = delegate.read();
            temp[0] = (byte) res;
            hex.dump(temp, 0, 1);
            return res;
        }
    }

    public static void main(String[] args) throws Exception {
        HexDumpWriter hdw = new HexDumpWriter(System.out);
        for (int i = 0; i < 1000; i++) {
            hdw.dump(CompTest1.makeRandomString((int) Math.round(Math.random() * 20)).getBytes());
            Thread.sleep(100);
        }
    }
}
