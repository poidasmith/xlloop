/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr.graph;

public class Edge<N>
{
    public final N source;
    public final N target;

    public Edge(N source, N target) {
        this.source = source;
        this.target = target;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof Edge))
            return false;

        Edge e = (Edge) obj;
        return e.source.equals(source) && e.target.equals(target);
    }

    public int hashCode() {
        return source.hashCode() ^ target.hashCode();
    }

    public String toString() {
        return "[" + source + "," + target + "]";
    }
}
