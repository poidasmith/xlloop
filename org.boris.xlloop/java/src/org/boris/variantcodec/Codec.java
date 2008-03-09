package org.boris.variantcodec;

import java.io.IOException;
import java.io.StringReader;

public class Codec {
    private int indent = 0;
    private int increment = 0;
    private int wrap = Integer.MAX_VALUE;
    private static Codec instance = new Codec();
    private static Codec formatted = new Codec(0, 4, 80);

    public Codec() {
    }

    public Codec(int indent, int increment, int wrap) {
        this.indent = indent;
        this.increment = increment;
        this.wrap = wrap;
    }

    public static Variant decode(String str) throws IOException {
        TokenReader tr = new TokenReader(new StringReader(str));
        char c = tr.ignoreWhitespace();
        switch (c) {
        case '{':
            return decodeStruct(tr);
        case '[':
            return decodeCollection(tr);
        case '\"':
            return decodeString(tr);
        default:
            StringBuilder sb = new StringBuilder();
            while (c != 0) {
                sb.append(c);
                c = (char) tr.read();
            }
            return decodeNumber(sb.toString());
        }
    }

    private static VTStruct decodeStruct(TokenReader tr) throws IOException {
        VTStruct s = new VTStruct();
        while (decodeField(tr, s))
            ;
        return s;
    }

    private static boolean decodeField(TokenReader tr, VTStruct s)
            throws IOException {
        char c = tr.ignoreWhitespace();
        if (c == '}')
            return false;

        String name = c + tr.readUntil('=');
        char l = tr.ignoreWhitespace();
        Variant dr = decodeValue(l, tr, ';');
        s.add(name, dr);

        return true;
    }

    private static Variant decodeValue(char firstChar, TokenReader tr,
            char endChar) throws IOException {
        Variant dr = null;
        switch (firstChar) {
        case ';':
            break;
        case '{':
            dr = decodeStruct(tr);
            tr.readUntil(endChar);
            break;
        case '[':
            dr = decodeCollection(tr);
            tr.readUntil(endChar);
            break;
        case '\"':
            dr = decodeString(tr);
            tr.readUntil(endChar);
            break;
        default:
            dr = decodeNumber(firstChar, endChar, tr);
            break;
        }

        return dr;
    }

    private static Variant decodeNumber(char firstChar, char endChar,
            TokenReader tr) throws IOException {
        return decodeNumber(firstChar + tr.readUntil(endChar));
    }

    private static Variant decodeNumber(String s) {
        if (s.indexOf('.') != -1) {
            return new VTDouble(Double.parseDouble(s));
        } else {
            return new VTLong(Long.parseLong(s));
        }
    }

    private static Variant decodeString(TokenReader tr) throws IOException {
        StringBuilder sb = new StringBuilder();
        char c = 0;
        while (c != '\"') {
            c = (char) tr.read();
            switch (c) {
            case '\\':
                sb.append((char) tr.read());
                break;
            case '\"':
                break;
            default:
                sb.append(c);
                break;
            }
        }
        return new VTString(sb.toString());
    }

    private static VTCollection decodeCollection(TokenReader tr)
            throws IOException {
        VTCollection coll = new VTCollection();
        char c = tr.ignoreWhitespace();
        while (c != ']') {
            boolean endblock = true;
            switch (c) {
            case '{':
                coll.add(decodeStruct(tr));
                break;
            case '[':
                coll.add(decodeCollection(tr));
                break;
            case '\"':
                coll.add(decodeString(tr));
                break;
            default:
                endblock = false;
                StringBuilder sb = new StringBuilder();
                while (c != ',' && c != ']') {
                    sb.append(c);
                    c = (char) tr.read();
                }
                coll.add(decodeNumber(sb.toString()));
                break;
            }

            if (endblock) {
                c = tr.ignoreWhitespace(); // read comma (or end block)
            }

            if (c == ']')
                return coll;
            c = tr.ignoreWhitespace();
        }
        return coll;
    }

    public static String encodeFormatted(Variant dr) {
        return formatted.encode(dr);
    }

    public static String encodeDefault(Variant dr) {
        return instance.encode(dr);
    }

    public String encode(Variant dr) {
        StringBuilder sb = new StringBuilder();
        indent = 0;
        encode(dr, sb);
        return sb.toString();
    }

    private void encode(Variant dr, StringBuilder sb) {
        if (indent > 0) {
            int width = 0;
            for (int i = sb.length() - 1; i >= 0; i--, width++) {
                if (sb.charAt(i) == '\n') {
                    if (width > wrap) {
                        sb.append('\n');
                        indent += increment;
                        appendIndent(sb);
                        indent -= increment;
                    }

                    break;
                }
            }
        }

        if (dr instanceof VTStruct) {
            encodeStruct((VTStruct) dr, sb);
        } else if (dr instanceof VTCollection) {
            encodeCollection((VTCollection) dr, sb);
        } else if (dr instanceof VTDouble) {
            encodeDouble((VTDouble) dr, sb);
        } else if (dr instanceof VTLong) {
            encodeLong((VTLong) dr, sb);
        } else if (dr instanceof VTString) {
            encodeString((VTString) dr, sb);
        }
    }

    private void encodeCollection(VTCollection c, StringBuilder sb) {
        sb.append("[");
        int size = c.size() - 1;
        for (int i = 0; i < size; i++) {
            encode(c.get(i), sb);
            sb.append(",");
        }
        if (size > 0) {
            encode(c.get(size), sb);
        }
        sb.append("]");
    }

    private void appendIndent(StringBuilder sb) {
        for (int i = 0; i < indent; i++) {
            sb.append(' ');
        }
    }

    private void encodeStruct(VTStruct s, StringBuilder sb) {
        sb.append("{");
        if (increment > 0)
            sb.append("\n");
        indent += increment;

        for (String str : s.getKeys()) {
            if (indent > 0)
                appendIndent(sb);
            sb.append(str);
            sb.append("=");
            encode(s.getValue(str), sb);
            sb.append(";");
            if (increment > 0)
                sb.append("\n");
        }

        if (increment > 0)
            sb.setLength(sb.length() - 1);

        indent -= increment;

        if (increment > 0)
            sb.append("\n");
        appendIndent(sb);
        sb.append("}");
    }

    private void encodeDouble(VTDouble d, StringBuilder sb) {
        sb.append(String.valueOf(d.doubleValue()));
    }

    private void encodeLong(VTLong l, StringBuilder sb) {
        sb.append(String.valueOf(l.longValue()));
    }

    private void encodeString(VTString str, StringBuilder sb) {
        sb.append("\"");
        encodeJavaString(str.get(), sb);
        sb.append("\"");
    }

    private void encodeJavaString(String str, StringBuilder sb) {
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
    }
}
