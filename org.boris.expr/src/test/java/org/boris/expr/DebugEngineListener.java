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

import org.boris.expr.engine.IEngineListener;

public class DebugEngineListener implements IEngineListener
{
    public void beforeCalculation(String name) {
        //System.out.printf("beforeCalculation: %s\n", name);
    }

    public void afterCalculation(String name, Expr value) {
        System.out.printf("afterCalculation: %s = %s\n", name, value);
    }
}
