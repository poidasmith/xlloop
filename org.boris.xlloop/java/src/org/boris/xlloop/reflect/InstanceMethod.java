package org.boris.xlloop.reflect;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.Variant;
import org.boris.xlloop.Function;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.util.VariantObjectConverter;

class InstanceMethod implements Function
{
    Class clazz;
    Object instance;
    Method method;
    VariantObjectConverter converter;
    Class[] args;
    
    public InstanceMethod(Class clazz, Object instance, Method method, VariantObjectConverter converter) {
        this.clazz = clazz;
        this.instance = instance;
        this.method = method;
        this.converter = converter;
        this.args = method.getParameterTypes();
    }
    
    public Variant execute(VTCollection args) throws RequestException {
        return converter.createFrom(execute(converter.convert(args, this.args)));
    }
    
    boolean matchesArgs(VTCollection args, int lastArg) throws RequestException {
        if(lastArg >= this.args.length) {
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