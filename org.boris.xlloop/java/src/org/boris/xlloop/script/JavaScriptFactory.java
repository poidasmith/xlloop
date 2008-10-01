/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.script;

import java.io.IOException;
import java.io.Reader;

import org.boris.xlloop.Function;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.util.XLoperObjectConverter;
import org.boris.xlloop.xloper.XLList;
import org.boris.xlloop.xloper.XLoper;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Script;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

public class JavaScriptFactory implements ScriptFactory
{
    private static XLoperObjectConverter converter = new XLoperObjectConverter();

    public Function create(Reader r) throws IOException {
        return new JavaScriptFunction(Context.enter().compileReader(r, null, 0,
                null), converter);
    }

    private static class JavaScriptFunction implements Function
    {
        private Script script;
        private XLoperObjectConverter converter;

        public JavaScriptFunction(Script script, XLoperObjectConverter converter) {
            this.script = script;
            this.converter = converter;
        }

        public XLoper execute(XLList args) throws RequestException {
            Context ctx = Context.enter();
            Object[] oargs = converter.convert(args, BSFScript
                    .createArgHints(args));
            ScriptableObject so = ctx.initStandardObjects();
            Scriptable argsObj = ctx.newArray(so, oargs);
            so.defineProperty("args", argsObj, ScriptableObject.DONTENUM);
            try {
                return converter.createFrom(script.exec(ctx, so));
            } catch (Throwable t) {
                throw new RequestException(t.getMessage());
            }
        }
    }
}
