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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.boris.xlloop.Function;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.util.XLList;
import org.boris.xlloop.util.XLoperObjectConverter;
import org.boris.xlloop.xloper.XLoper;

class InstanceMethod implements Function
{
    Class clazz;
    Object instance;
    Method method;
    XLoperObjectConverter converter;
    Class[] args;

    public InstanceMethod(Class clazz, Object instance, Method method,
            XLoperObjectConverter converter) {
        this.clazz = clazz;
        this.instance = instance;
        this.method = method;
        this.converter = converter;
        this.args = method.getParameterTypes();
    }

    public XLoper execute(XLList args) throws RequestException {
        return converter
                .createFrom(execute(converter.convert(args, this.args)));
    }

    boolean matchesArgs(XLList args, int lastArg) throws RequestException {
        if (lastArg >= this.args.length) {
            return false;
        }
        return args != null;
    }

    Object execute(Object[] args) throws RequestException {
        try {
            return method.invoke(instance, (Object[]) args);
        } catch (IllegalArgumentException e) {
            throw new RequestException(e);
        } catch (IllegalAccessException e) {
            throw new RequestException(e);
        } catch (InvocationTargetException e) {
            throw new RequestException(e.getTargetException());
        }
    }
}