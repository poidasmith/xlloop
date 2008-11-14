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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

public class ExprLexer
{
    private TokenReader reader;
    private int lastChar;
    private ExprToken next;

    public ExprLexer(BufferedReader reader) {
        this.reader = new TokenReader(reader);
    }

    public ExprLexer(Reader reader) {
        this(new BufferedReader(reader));
    }

    public ExprLexer(String str) {
        this(new StringReader(str));
    }

    public ExprToken next() throws IOException {
        if (next != null) {
            ExprToken ret = next;
            next = null;
            return ret;
        }

        if (lastChar == 0 || Character.isWhitespace(lastChar))
            lastChar = reader.ignoreWhitespace();

        return readToken();
    }

    private ExprToken readToken() throws IOException {
        if (Character.isDigit(lastChar)) {
            return readNumber();
        }

        switch (lastChar) {
        case '\"':
            return readString();
        case '(':
            lastChar = 0;
            return ExprToken.OPEN_BRACKET;
        case ')':
            lastChar = 0;
            return ExprToken.CLOSE_BRACKET;
        case '+':
            lastChar = 0;
            return ExprToken.PLUS;
        case '-':
            lastChar = 0;
            return ExprToken.MINUS;
        case '*':
            lastChar = 0;
            return ExprToken.MULTIPLY;
        case '/':
            lastChar = 0;
            return ExprToken.DIVIDE;
        case ',':
            lastChar = 0;
            return ExprToken.COMMA;
        case '&':
            lastChar = 0;
            return ExprToken.STRING_CONCAT;
        case -1:
        case 0xffff:
            return null;
        }

        if (!Character.isJavaIdentifierStart(lastChar)) {
            throw new IOException("Invalid token found: " + lastChar);
        }

        ExprToken varF = readVariableOrFunction();

        // Need to peek at next token to see if its an open bracket -
        // if so then we have function otherwise a variable
        next = next();

        if (next != null && next.type.equals(ExprTokenType.OpenBracket)) {
            next = null; // We don't need the open bracket as we know it must
            // be there
            return new ExprToken(ExprTokenType.Function, varF.val);
        } else {
            return varF;
        }
    }

    private ExprToken readVariableOrFunction() throws IOException {
        StringBuilder sb = new StringBuilder();

        while (isVariablePart(lastChar)) {
            sb.append((char) lastChar);
            lastChar = reader.read();
        }

        return new ExprToken(ExprTokenType.Variable, sb.toString());
    }

    private boolean isVariablePart(int lastChar) {
        return Character.isJavaIdentifierPart(lastChar) || lastChar == '!' ||
                lastChar == ':';
    }

    private ExprToken readString() throws IOException {
        String str = unescapeJavaString(reader);
        lastChar = 0;
        return new ExprToken(ExprTokenType.String, str);
    }

    private ExprToken readNumber() throws IOException {
        StringBuilder sb = new StringBuilder(); // Todo, more efficient number
        // builder (ie. shift bits)
        sb.append((char) lastChar);
        lastChar = reader.read();
        boolean decimal = false;
        while (Character.isDigit(lastChar) || '.' == lastChar) {
            sb.append((char) lastChar);
            if (lastChar == '.')
                decimal = true;
            lastChar = reader.read();
        }

        String val = sb.toString();
        if (decimal) {
            return new ExprToken(val, Double.parseDouble(val));
        } else {
            return new ExprToken(val, Integer.parseInt(val));
        }
    }

    public static String escapeJavaString(String str) {
        StringBuilder sb = new StringBuilder();
        int len = str.length();
        for (int i = 0; i < len; i++) {
            char c = str.charAt(i);
            switch (c) {
            case '\\':
                sb.append("\\\\");
                break;
            case '\"':
                sb.append("\\\"");
                break;
            default:
                sb.append(c);
                break;
            }
        }
        return sb.toString();
    }

    public static String unescapeJavaString(Reader r) throws IOException {
        StringBuilder sb = new StringBuilder();
        char c = 0;
        while (c != '\"') {
            c = (char) r.read();
            switch (c) {
            case '\\':
                sb.append((char) r.read());
                break;
            case '\"':
                break;
            default:
                sb.append(c);
                break;
            }
        }
        return sb.toString();
    }
}
