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

import org.boris.xlloop.IFunction;
import org.boris.xlloop.IFunctionContext;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.xloper.XLoper;

public class DelegateFunction implements IFunction
{
    private Object object;
    private Method method;

    public DelegateFunction(Class clazz, Object object, String method) throws SecurityException, NoSuchMethodException {
        this.object = object;
        this.method = clazz.getMethod(method, new Class[] { XLoper[].class });
    }

    public DelegateFunction(Object object, Method method) {
        this.object = object;
        this.method = method;
    }

    public String getName() {
        return method.getName();
    }

    public XLoper execute(IFunctionContext context, XLoper[] args) throws RequestException {
        try {
            return (XLoper) method.invoke(object, new Object[] { args });
        } catch (Exception e) {
            if (e instanceof RequestException)
                throw (RequestException) e;
            throw new RequestException(e);
        }
    }

}
