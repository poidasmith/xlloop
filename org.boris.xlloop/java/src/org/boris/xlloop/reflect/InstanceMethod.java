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
import org.boris.xlloop.util.XLoperObjectConverter;
import org.boris.xlloop.xloper.XLBool;
import org.boris.xlloop.xloper.XLError;
import org.boris.xlloop.xloper.XLInt;
import org.boris.xlloop.xloper.XLNum;
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

    public XLoper execute(XLoper[] args) throws RequestException {
        return converter
                .createFrom(execute(converter.convert(args, this.args)));
    }

    boolean matchesArgs(XLoper[] args, int lastArg) throws RequestException {
        if (lastArg >= this.args.length) {
            return false;
        }

        return args != null;
    }

    double calcMatchPercent(XLoper[] args, int lastArg) throws RequestException {
        if (lastArg >= this.args.length) {
            return 0;
        }

        double calc = 0;
        int i = 0;
        for (; i < args.length && i < this.args.length; i++) {
            calc += calcMatchPercent(args[i], this.args[i]);
        }

        return calc / i;
    }

    double calcMatchPercent(XLoper arg, Class c) throws RequestException {
        if (c == null)
            return 100;
        switch (arg.type) {
        case XLoper.xlTypeBool:
            if (c == Boolean.class || c == XLBool.class)
                return 100;
            else if (c.isAssignableFrom(Number.class))
                return 50;
            break;
        case XLoper.xlTypeErr:
            if (c == XLError.class)
                return 100;
            break;
        case XLoper.xlTypeInt:
            if (c == Integer.class || c == XLInt.class)
                return 100;
            else if (c.isAssignableFrom(Number.class))
                return 50;
            break;
        case XLoper.xlTypeMulti:
            break;
        case XLoper.xlTypeNum:
            if (c == double.class || c == Double.class || c == XLNum.class)
                return 100;
            else if (c.isAssignableFrom(Number.class) || c == int.class ||
                    c == long.class || c == float.class)
                return 50;
            break;
        case XLoper.xlTypeStr:
            // Design bug - we don't know at this point
            return 100;
        }

        return 0;
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