package org.boris.functionserver.script;

import org.apache.bsf.BSFManager;
import org.boris.functionserver.Function;
import org.boris.functionserver.RequestException;
import org.boris.functionserver.util.VariantObjectConverter;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;

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

    public Variant execute(VTCollection args) throws RequestException {
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
        try {
            Object[] a = converter.convert(args, hints);
            BSFManager manager = new BSFManager();
            manager.declareBean("args", a, Object[].class);
            Object res = manager.eval(lang, name, 1, 1, source);
            return converter.createFrom(res);
        } catch (Throwable e) {
            throw new RequestException(e);
        }
    }
}
