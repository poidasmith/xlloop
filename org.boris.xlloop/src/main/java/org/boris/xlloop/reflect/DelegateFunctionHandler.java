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
import java.util.List;

import org.boris.xlloop.FunctionMap;
import org.boris.xlloop.handler.FunctionInformation;
import org.boris.xlloop.handler.FunctionProvider;

public class DelegateFunctionHandler extends FunctionMap implements FunctionProvider
{
    private List<FunctionInformation> functions = new ArrayList();

    public void addMethods(String namespace, Class clazz) {
        if (namespace == null)
            namespace = "";

        Method[] methods = clazz.getMethods();
        for (Method m : methods) {
            int mod = m.getModifiers();
            if (Modifier.isPublic(mod) && Modifier.isStatic(mod))
                add(namespace + m.getName(), m);
        }

    }

    private void add(String name, Method m) {
        XLFunction xf = m.getAnnotation(XLFunction.class);
        FunctionInformation fi = AnnotationHelper.extract(name, xf);
        if (fi != null) {
            name = fi.getName();
            functions.add(fi);
        }

        add(name, new DelegateFunction(null, m));
    }

    public FunctionInformation[] getFunctions() {
        return functions.toArray(new FunctionInformation[functions.size()]);
    }
}
