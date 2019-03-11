/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.reflect;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.boris.xlloop.IFunction;
import org.boris.xlloop.IFunctionContext;
import org.boris.xlloop.IFunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.handler.FunctionInformation;
import org.boris.xlloop.handler.FunctionProvider;
import org.boris.xlloop.util.XLoperObjectConverter;
import org.boris.xlloop.xloper.XLoper;

public class ReflectFunctionHandler implements IFunctionHandler, FunctionProvider
{
    private Map methods = new HashMap();
    private XLoperObjectConverter converter = new XLoperObjectConverter();
    private Map information = new HashMap();

    public Set getFunctionList() {
        return methods.keySet();
    }

    public void addMethod(Object instance, Method m) {
        addMethod(m.getName(), instance.getClass(), instance, m);
    }

    public void addInformation(String name, FunctionInformation fi) {
        information.put(name, fi);
    }

    public void addMethod(String name, Class c, Object instance, Method m) {
        if (!m.getDeclaringClass().equals(c))
            return;
        XLFunction xf = m.getAnnotation(XLFunction.class);
        FunctionInformation fi = AnnotationHelper.extract(name, xf);
        if (fi != null) {
            name = fi.getName();
            information.put(name, fi);
        }
        IFunction f = (IFunction) methods.get(name);
        if (f instanceof InstanceMethod) {
            OverloadedMethod om = new OverloadedMethod();
            om.add((InstanceMethod) f);
            om.add(new InstanceMethod(c, instance, m, converter));
            methods.put(name, om);
        } else if (f instanceof OverloadedMethod) {
            ((OverloadedMethod) f).add(new InstanceMethod(c, instance, m, converter));
        } else {
            methods.put(name, new InstanceMethod(c, instance, m, converter));
        }
    }

    public void addMethods(String namespace, Class c) {
        Method[] m = c.getMethods();
        for (int i = 0; i < m.length; i++) {
            if (Modifier.isStatic(m[i].getModifiers())) {
                if (namespace == null) {
                    addMethod(null, m[i]);
                } else {
                    addMethod(namespace + m[i].getName(), c, null, m[i]);
                }
            }
        }
    }

    public void addMethods(String namespace, Object instance) {
        Method[] m = instance.getClass().getMethods();
        for (int i = 0; i < m.length; i++) {
            if ((instance == null && Modifier.isStatic(m[i].getModifiers())) || instance != null) {
                if (namespace == null) {
                    addMethod(instance, m[i]);
                } else {
                    addMethod(namespace + m[i].getName(), instance.getClass(), instance, m[i]);
                }
            }
        }
    }

    public XLoper execute(IFunctionContext context, String name, XLoper[] args) throws RequestException {
        IFunction f = (IFunction) methods.get(name);
        if (f == null) {
            throw new RequestException("#Unknown method: " + name);
        }
        return f.execute(context, args);
    }

    public boolean hasFunction(String name) {
        return methods.containsKey(name);
    }

    public FunctionInformation[] getFunctions() {
        ArrayList functions = new ArrayList();
        for (Iterator i = methods.keySet().iterator(); i.hasNext();) {
            String key = (String) i.next();
            FunctionInformation fi = (FunctionInformation) information.get(key);
            if (fi != null) {
                functions.add(fi);
                continue;
            }
            IFunction f = (IFunction) methods.get(key);
            fi = new FunctionInformation(key);
            if (f instanceof InstanceMethod) {
                try {
                    InstanceMethod im = (InstanceMethod) f;
                    ParameterNameExtractor pne = new ParameterNameExtractor(im.clazz);
                    String[] names = pne.getParameterNames(im.method);
                    for (int j = 0; j < names.length; j++) {
                        fi.addArgument(names[j], im.args[j].getSimpleName());
                    }
                } catch (Exception e) {
                }
            } else if (f instanceof OverloadedMethod) {
                try {
                    OverloadedMethod om = (OverloadedMethod) f;
                    InstanceMethod im = om.getFirstMethod();
                    ParameterNameExtractor pne = new ParameterNameExtractor(im.clazz);
                    String[] names = pne.getParameterNames(im.method);
                    for (int j = 0; j < names.length; j++) {
                        fi.addArgument(names[j], im.args[j].getSimpleName());
                    }
                } catch (Exception e) {
                }
            }
            functions.add(fi);
        }
        return (FunctionInformation[]) functions.toArray(new FunctionInformation[0]);
    }
}
