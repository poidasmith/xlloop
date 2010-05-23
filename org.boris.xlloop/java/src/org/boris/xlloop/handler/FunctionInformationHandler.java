/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.handler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.boris.xlloop.IBuiltinFunctions;
import org.boris.xlloop.IFunctionContext;
import org.boris.xlloop.IFunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.util.XLList;
import org.boris.xlloop.xloper.XLoper;

public class FunctionInformationHandler implements IFunctionHandler, IBuiltinFunctions
{
    private ArrayList functions = new ArrayList();
    private Set functionProviders = new HashSet();
    private String category;

    public void add(FunctionInformation fi) {
        functions.add(fi);
    }

    public void add(FunctionInformation[] fis) {
        if (fis != null)
            functions.addAll(Arrays.asList(fis));
    }

    public void add(FunctionProvider prov) {
        functionProviders.add(prov);
    }

    public void remove(FunctionProvider prov) {
        functionProviders.remove(prov);
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public String getCategory() {
        return category;
    }

    public XLoper execute(IFunctionContext context, String name, XLoper[] args) throws RequestException {
        XLList c = new XLList();
        Map m = new TreeMap();
        for (Iterator i = functions.iterator(); i.hasNext();) {
            FunctionInformation fi = (FunctionInformation) i.next();
            System.out.println(fi.getName());
            m.put(fi.getName(), fi);
        }

        for (Iterator i = functionProviders.iterator(); i.hasNext();) {
            FunctionProvider fp = (FunctionProvider) i.next();
            FunctionInformation[] fis = fp.getFunctions();
            if (fis != null) {
                for (int j = 0; j < fis.length; j++) {
                    System.out.println(fis[j].getName());
                    m.put(fis[j].getName(), fis[j]);
                }
            }
        }

        for (Iterator i = m.keySet().iterator(); i.hasNext();) {
            FunctionInformation fi = (FunctionInformation) m.get(i.next());
            if (category != null && fi.getCategory() == null) {
                fi.setCategory(category);
            }
            c.add(fi.encode());
        }

        return c.toXLoper();
    }

    public boolean hasFunction(String name) {
        return GET_FUNCTIONS.equals(name);
    }
}
