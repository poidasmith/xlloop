/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr.parser;

import java.io.IOException;
import java.io.Reader;

public class TokenReader extends Reader
{
    private Reader r;

    public TokenReader(Reader r) {
        this.r = r;
    }

    public void close() throws IOException {
        r.close();
    }

    public int read(char[] cbuf, int off, int len) throws IOException {
        return r.read(cbuf, off, len);
    }

    public char ignoreWhitespace() throws IOException {
        while (true) {
            char c = (char) r.read();
            if (!Character.isWhitespace(c)) {
                return c;
            }
        }
    }

    public String readUntil(char token) throws IOException {
        StringBuilder sb = new StringBuilder();
        char c = 0;
        while ((c = (char) r.read()) != token) {
            sb.append(c);
        }
        return sb.toString();
    }
}
