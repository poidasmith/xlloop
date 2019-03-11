/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.http;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import org.boris.xlloop.IFunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.xloper.XLArray;
import org.boris.xlloop.xloper.XLBool;
import org.boris.xlloop.xloper.XLError;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLMissing;
import org.boris.xlloop.xloper.XLNil;
import org.boris.xlloop.xloper.XLNum;
import org.boris.xlloop.xloper.XLSRef;
import org.boris.xlloop.xloper.XLString;
import org.boris.xlloop.xloper.XLoper;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

public class JSONCodec
{
    public static void doRequest(IFunctionHandler handler, Reader input, Writer output) throws IOException,
            JSONException {
        BufferedWriter bw = new BufferedWriter(output);
        try {
            FunctionRequest fr = JSONCodec.decodeRequest(input);
            System.out.println(JSONCodec.encodeRequest(fr).toString(4));
            XLoper res = handler.execute(null, fr.getName(), fr.getArgs());
            System.out.println(JSONCodec.encode(res).toString(4));
            JSONCodec.encodeXLoper(res, bw);
        } catch (RequestException e) {
            e.printStackTrace();
            XLoper res = new XLString(e.getMessage());
            JSONCodec.encodeXLoper(res, bw);
            bw.flush();
        } finally {
            if (bw != null)
                bw.flush();
        }
    }

    public static FunctionRequest decodeRequest(Reader r) throws IOException, JSONException {
        JSONTokener t = new JSONTokener(r);
        JSONObject jo = new JSONObject(t);
        String name = jo.getString("name");
        JSONArray args = jo.getJSONArray("args");
        String sheetName = (String) jo.opt("sheetName");
        JSONObject caller = (JSONObject) jo.opt("caller");
        XLSRef cref = null;
        if (caller != null) {
            cref = (XLSRef) decode(caller);
        }
        XLoper[] xargs = new XLoper[args.length()];
        for (int i = 0; i < xargs.length; i++) {
            xargs[i] = decode(args.getJSONObject(i));
        }

        return new FunctionRequest(name, xargs, cref, sheetName);

    }

    public static XLoper decodeXLoper(Reader r) throws IOException, JSONException {
        JSONTokener t = new JSONTokener(r);
        JSONObject jo = new JSONObject(t);
        return decode(jo);
    }

    public static void encodeRequest(FunctionRequest fr, Writer w) throws IOException, JSONException {
        encodeRequest(fr).write(w);
    }

    public static JSONObject encodeRequest(FunctionRequest fr) throws IOException, JSONException {
        JSONObject o = new JSONObject();
        o.put("request", "XLLoop");
        o.put("version", "0.1.0");
        o.put("name", fr.getName());
        if (fr.getSheetName() != null)
            o.put("sheetName", fr.getSheetName());
        if (fr.getCaller() != null)
            o.put("caller", encode(fr.getCaller()));
        JSONArray a = new JSONArray();
        XLoper[] xargs = fr.getArgs();
        if (xargs != null) {
            for (int i = 0; i < xargs.length; i++) {
                a.put(encode(xargs[i]));
            }
        }
        o.put("args", a);
        return o;
    }

    public static void encodeXLoper(XLoper x, Writer w) throws IOException, JSONException {
        JSONObject o = encode(x);
        o.write(w);
    }

    public static JSONObject encode(XLoper x) throws JSONException {
        JSONObject o = new JSONObject();
        if (x == null) {
            o.put("type", XLoper.xlTypeNil);
            return o;
        }
        o.put("type", x.type);
        switch (x.type) {
        case XLoper.xlTypeBool:
            o.put("bool", ((XLBool) x).bool);
            break;
        case XLoper.xlTypeErr:
            o.put("error", ((XLError) x).err);
            break;
        case XLoper.xlTypeInt:
            o.put("int", ((XLInt) x).w);
            break;
        case XLoper.xlTypeMissing:
            break;
        case XLoper.xlTypeMulti:
            XLArray a = (XLArray) x;
            o.put("rows", a.rows);
            o.put("cols", a.columns);
            JSONArray ja = new JSONArray();
            for (int i = 0; i < a.length; i++) {
                ja.put(encode(a.array[i]));
            }
            o.put("array", ja);
            break;
        case XLoper.xlTypeNil:
            break;
        case XLoper.xlTypeNum:
            o.put("num", ((XLNum) x).num);
            break;
        case XLoper.xlTypeStr:
            o.put("str", ((XLString) x).str);
            break;
        case XLoper.xlTypeSRef:
            o.put("colFirst", ((XLSRef) x).colFirst);
            o.put("colLast", ((XLSRef) x).colLast);
            o.put("rowFirst", ((XLSRef) x).rwFirst);
            o.put("rowLast", ((XLSRef) x).rwLast);
        }
        return o;
    }

    private static XLoper decode(JSONObject jo) throws JSONException {
        switch (jo.getInt("type")) {
        case XLoper.xlTypeBool:
            return jo.getBoolean("bool") ? XLBool.TRUE : XLBool.FALSE;
        case XLoper.xlTypeErr:
            return new XLError(jo.getInt("error"));
        case XLoper.xlTypeInt:
            return new XLInt(jo.getInt("int"));
        case XLoper.xlTypeMissing:
            return XLMissing.MISSING;
        case XLoper.xlTypeMulti:
            int rows = jo.getInt("rows");
            int cols = jo.getInt("cols");
            int len = rows * cols;
            XLoper[] a = new XLoper[len];
            JSONArray ja = jo.getJSONArray("array");
            for (int i = 0; i < len; i++) {
                a[i] = decode(ja.getJSONObject(i));
            }
            return new XLArray(a, rows, cols);
        case XLoper.xlTypeNil:
            return XLNil.NIL;
        case XLoper.xlTypeNum:
            return new XLNum(jo.getDouble("num"));
        case XLoper.xlTypeStr:
            return new XLString(jo.getString("str"));
        case XLoper.xlTypeSRef:
            return new XLSRef(jo.getInt("colFirst"), jo.getInt("colLast"), jo.getInt("rowFirst"), jo.getInt("rowLast"));
        }

        return null;
    }
}
