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

import org.apache.bsf.BSFManager;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.Function;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.util.VariantObjectConverter;

public class BSFScript implements Function
{
    private VariantObjectConverter converter = new VariantObjectConverter();
    private String lang;
    private String source;
    private String name;

    public BSFScript(String lang, String source, String name) {
        this.lang = lang;
        this.source = source;
        this.name = name;
    }

    public static Class[] createArgHints(VTCollection args) {
        Class[] hints = new Class[args.size()];
        for (int i = 0; i < hints.length; i++) {
            Variant v = args.get(i);
            if(v instanceof VTCollection) {
                VTCollection c = (VTCollection) v;
                if(c.size() > 0 && c.get(0) instanceof VTCollection) {
                    hints[i] = Object[][].class;
                } else {
                    hints[i] = Object[].class;
                }
            } else {
                hints[i] = Object.class;
            }
        }
        return hints;
    }
    
    public Variant execute(VTCollection args) throws RequestException {
        try {
            Object[] a = converter.convert(args, createArgHints(args));
            BSFManager manager = new BSFManager();
            manager.declareBean("args", a, Object[].class);
            Object res = manager.eval(lang, name, 1, 1, source);
            return converter.createFrom(res);
        } catch (Throwable e) {
            throw new RequestException(e);
        }
    }
}
