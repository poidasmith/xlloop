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
import java.util.Set;

import org.boris.variant.VTCollection;
import org.boris.variant.VTMap;
import org.boris.variant.Variant;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.RequestHandler;

public class FunctionInformationRequestHandler implements RequestHandler
{
    private ArrayList functions = new ArrayList();
    private Set functionProviders = new HashSet();
    
    public void add(FunctionInformation fi) {
        functions.add(fi);
    }
    
    public void add(FunctionInformation[] fis) {
        functions.addAll(Arrays.asList(fis));
    }
    
    public void add(FunctionProvider prov) {
        functionProviders.add(prov);
    }
    
    public Variant execute(String name, VTMap args) throws RequestException {
        VTCollection c = new VTCollection();
        for(Iterator i = functions.iterator(); i.hasNext(); ) {
            FunctionInformation fi = (FunctionInformation) i.next();
            c.add(fi.encode());
        }
        for(Iterator i = functionProviders.iterator(); i.hasNext(); ){
            FunctionProvider fp = (FunctionProvider) i.next();
            FunctionInformation[] fis = fp.getFunctions();
            if(fis != null) {
                for(int j = 0; j < fis.length; j++) {
                    c.add(fis[j].encode());
                }
            }
        }
        return c;
    }

    public boolean hasRequest(String name) {
        return "GetFunctions".equals(name);
    }
}
