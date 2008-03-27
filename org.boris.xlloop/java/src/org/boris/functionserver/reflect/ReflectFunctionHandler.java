package org.boris.functionserver.reflect;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.boris.functionserver.Function;
import org.boris.functionserver.FunctionHandler;
import org.boris.functionserver.RequestException;
import org.boris.functionserver.util.VariantObjectConverter;
import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;

public class ReflectFunctionHandler implements FunctionHandler
{
    private Map methods = new HashMap();
    private VariantObjectConverter converter = new VariantObjectConverter();

    public Set getFunctionList() {
        return methods.keySet();
    }

    public void addMethod(Object instance, Method m) {
        addMethod(m.getName(), instance, m);
    }

    public void addMethod(String name, Object instance, Method m) {
        Function f = (Function) methods.get(name);
        if (f instanceof InstanceMethod) {
            OverloadedMethod om = new OverloadedMethod();
            om.add((InstanceMethod) f);
            om.add(new InstanceMethod(instance, m, converter));
            methods.put(name, om);
        } else if (f instanceof OverloadedMethod) {
            ((OverloadedMethod) f).add(new InstanceMethod(instance, m,
                    converter));
        } else {
            methods.put(name, new InstanceMethod(instance, m, converter));
        }
    }

    public void addMethods(String namespace, Class c) {
        Method[] m = c.getMethods();
        for (int i = 0; i < m.length; i++) {
            if (Modifier.isStatic(m[i].getModifiers())) {
                if (namespace == null) {
                    addMethod(null, m[i]);
                } else {
                    addMethod(namespace + m[i].getName(), null, m[i]);
                }
            }
        }
    }

    public void addMethods(String namespace, Object instance) {
        Method[] m = instance.getClass().getMethods();
        for (int i = 0; i < m.length; i++) {
            if ((instance == null && Modifier.isStatic(m[i].getModifiers())) ||
                    instance != null) {
                if (namespace == null) {
                    addMethod(instance, m[i]);
                } else {
                    addMethod(namespace + m[i].getName(), instance, m[i]);
                }
            }
        }
    }

    public Variant execute(String name, VTCollection args)
            throws RequestException {
        Function f = (Function) methods.get(name);
        if (f == null) {
            throw new RequestException("#Unknown method: " + name);
        }
        return f.execute(args);
    }

    public boolean hasFunction(String name) {
        return methods.containsKey(name);
    }
}
