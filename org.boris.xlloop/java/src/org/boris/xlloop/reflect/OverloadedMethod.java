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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.boris.xlloop.Function;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.xloper.XLMissing;
import org.boris.xlloop.xloper.XLNil;
import org.boris.xlloop.xloper.XLoper;

public class OverloadedMethod implements Function
{
    private List methods = new ArrayList();

    public void add(InstanceMethod m) {
        this.methods.add(m);
    }

    public XLoper execute(XLoper[] args) throws RequestException {
        int lastArg = args.length - 1;
        for (; lastArg >= 0; lastArg--) {
            if (!(args[lastArg] instanceof XLNil || args[lastArg] instanceof XLMissing)) {
                break;
            }
        }

        InstanceMethod matched = null;
        double matchPercent = 0;

        for (Iterator i = methods.iterator(); i.hasNext();) {
            InstanceMethod m = (InstanceMethod) i.next();
            double mc = m.calcMatchPercent(args, lastArg);
            if (mc > 0 && mc > matchPercent) {
                matched = m;
                matchPercent = mc;
            }
        }

        if (matched != null)
            return matched.execute(args);

        throw new RequestException("#Invalid args");
    }

    public InstanceMethod getFirstMethod() {
        return (InstanceMethod) methods.get(0);
    }
}
