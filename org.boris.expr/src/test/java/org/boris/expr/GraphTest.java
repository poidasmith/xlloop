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

import org.boris.expr.graph.Edge;
import org.boris.expr.graph.Graph;

public class GraphTest extends TH
{
    public void testCheckCycle() throws Exception {
        Graph g = new Graph();
        g.add(new Edge("a", "b"));
        g.add(new Edge("a", "c"));
        g.add(new Edge("b", "d"));
        g.add(new Edge("c", "d"));
    }
}
