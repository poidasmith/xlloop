/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.boris.expr.engine.ListenerEngine;
import org.boris.expr.function.ExcelFunctionProvider;

public class ListenerEngineTest
{
    public static void main(String[] args) throws Exception {
        ListenerEngine le = new ListenerEngine(new ExcelFunctionProvider());
        le.addListener(new DebugEngineListener());
        le.set("a", "b+c");
        le.set("c", "sin(123+b)");
        le.set("b", "54.32");
        ExecutorService es = Executors.newFixedThreadPool(5);
        le.startCalculation(es, 5);
        Thread.sleep(500);
        le.set("d", "a*b");
        le.set("b", "2.");
    }
}
