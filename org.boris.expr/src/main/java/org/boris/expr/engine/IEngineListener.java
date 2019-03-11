/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr.engine;

import org.boris.expr.Expr;

public interface IEngineListener
{
    void beforeCalculation(String name);

    void afterCalculation(String name, Expr value);
}
