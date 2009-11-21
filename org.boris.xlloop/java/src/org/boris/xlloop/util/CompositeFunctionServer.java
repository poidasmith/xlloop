/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.util;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.boris.xlloop.IFunctionContext;
import org.boris.xlloop.IFunctionHandler;
import org.boris.xlloop.RequestException;
import org.boris.xlloop.RequestExecutor;
import org.boris.xlloop.xloper.XLoper;

/**
 * A basic function server that hosts multiple child request executors and
 * delegates requests (in a round-robin fashion).
 */
public class CompositeFunctionServer implements IFunctionHandler
{
    private Collection executors;
    private volatile CircularIterator iterator;

    public CompositeFunctionServer(RequestExecutor[] executors) {
        this.executors = new ConcurrentLinkedQueue(Arrays.asList(executors));
        this.iterator = new CircularIterator(this.executors);
    }

    public void add(RequestExecutor re) {
        executors.add(re);
        iterator = new CircularIterator(executors);
    }

    public void remove(RequestExecutor re) {
        executors.remove(re);
        iterator = new CircularIterator(executors);
    }

    public XLoper execute(IFunctionContext context, String name, XLoper[] args) throws RequestException {
        try {
            return ((RequestExecutor) iterator.next()).execute(name, args);
        } catch (IOException e) {
            throw new RequestException(e);
        }
    }

    public boolean hasFunction(String name) {
        return true;
    }
}
