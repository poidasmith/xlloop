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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Graph<N>
{
    private Set<N> nodes = new HashSet();
    private Set<Edge<N>> edges = new HashSet();
    private Map<N,Set<Edge<N>>> outbounds = new HashMap();
    private Map<N,Set<Edge<N>>> inbounds = new HashMap();

    public void add(N node) {
        nodes.add(node);
    }

    public Set<Edge<N>> getInbounds(N node) {
        return inbounds.get(node);
    }

    public Set<Edge<N>> getOutbounds(N node) {
        return outbounds.get(node);
    }

    public void clearOutbounds(N node) {
        Set<Edge<N>> s = outbounds.get(node);
        if (s != null) {
            Iterator<Edge<N>> i = s.iterator();
            while (i.hasNext())
                remove(i.next());
        }
    }

    public void clearInbounds(N node) {
        Set<Edge<N>> s = inbounds.get(node);
        if (s != null) {
            Iterator<Edge<N>> i = s.iterator();
            while (i.hasNext())
                remove(i.next());
        }
    }

    public void remove(N node) {
        nodes.remove(node);
        clearInbounds(node);
        clearOutbounds(node);
    }

    public void add(Edge<N> e) throws GraphCycleException {
        checkCycle(e);
        nodes.add(e.source);
        nodes.add(e.target);
        edges.add(e);
        Set in = (Set) inbounds.get(e.target);
        if (in == null)
            inbounds.put(e.target, in = new HashSet());
        in.add(e);
        Set out = (Set) outbounds.get(e.source);
        if (out == null)
            outbounds.put(e.source, out = new HashSet());
        out.add(e);
    }

    public void checkCycle(Edge<N> e) throws GraphCycleException {
        Set<N> visited = new HashSet();
        visited.add(e.source);
        checkCycle(e, visited);
    }

    private void checkCycle(Edge<N> e, Set<N> visited) throws GraphCycleException {
        if (visited.contains(e.target)) {
            throw new GraphCycleException("Circular reference found: " +
                    e.source + " - " + e.target);
        }
        visited.add(e.target);
        Set<Edge<N>> out = outbounds.get(e.target);
        if (out != null) {
            Iterator<Edge<N>> i = out.iterator();
            while (i.hasNext()) {
                checkCycle(i.next(), visited);
            }
        }
    }

    public void remove(Edge<N> e) {
        edges.remove(e);
        Set<Edge<N>> in = inbounds.get(e.target);
        if (in != null)
            in.remove(e);
        Set out = (Set) outbounds.get(e.source);
        if (out != null)
            out.remove(e);
    }

    public List<N> sort() {
        List<N> ordered = new ArrayList();
        Set<N> traversed = new HashSet();
        Iterator<N> i = nodes.iterator();
        Set<N> remains = new HashSet(nodes);

        // First traverse nodes without inbounds
        while (i.hasNext()) {
            N n = i.next();
            Set in = (Set) inbounds.get(n);
            if (in == null || in.isEmpty()) {
                traverse(n, traversed, ordered);
                remains.remove(n);
            }
        }

        // Now traverse the rest
        i = remains.iterator();
        while (i.hasNext()) {
            N n = i.next();
            if (!traversed.contains(n)) {
                traverse(n, traversed, ordered);
            }
        }
        
        return ordered;
    }



    public void clear() {
        edges.clear();
        inbounds.clear();
        outbounds.clear();
        nodes.clear();
    }

    public void traverse(N node, GraphTraversalListener<N> listener) {
        HashSet subgraph = new HashSet();
        walk(node, subgraph);
        HashSet hs = new HashSet();
        hs.add(node);
        traverse(node, listener, hs, subgraph);
    }

    private void walk(N node, Set<N> traversed) {
        traversed.add(node);
        Set<Edge<N>> out = outbounds.get(node);
        if (out != null) {
            Iterator<Edge<N>> i = out.iterator();
            while (i.hasNext()) {
                N n = i.next().target;
                if(!traversed.contains(n))
                    walk(n, traversed);
            }
        }
    }

    private void traverse(N node, Set<N> traversed, List<N> ordered) {
        Set<Edge<N>> in = inbounds.get(node);
        if (in != null) {
            Iterator<Edge<N>> i = in.iterator();

            // if all inbounds haven't been traversed we must stop
            while (i.hasNext()) {
                Edge<N> e = i.next();
                if (!traversed.contains(e.source))
                    return;
            }
        }

        if (!traversed.contains(node)) {
            traversed.add(node);
            ordered.add(node);
        }

        Set out = (Set) outbounds.get(node);
        if (out == null || out.isEmpty()) {
            return;
        }

        Set<N> avoid = new HashSet();

        Iterator<Edge<N>> i = out.iterator();
        while (i.hasNext()) {
            Edge<N> e = i.next();
            if (!traversed.contains(e)) {
                if (traversed.contains(e.target)) {
                    avoid.add(e.target);
                }
            }
        }

        i = out.iterator();
        while (i.hasNext()) {
            N n = i.next().target;
            if (!avoid.contains(n)) {
                traverse(n, traversed, ordered);
            }
        }
    }
    
    private void traverse(N node, GraphTraversalListener<N> listener,
            Set<N> traversed, Set<N> subgraph) {
        Set<Edge<N>> edges = outbounds.get(node);
        if (edges != null) {
            Iterator<Edge<N>> i = edges.iterator();
            while (i.hasNext()) {
                Edge<N> e = i.next();
                Set<Edge<N>> ins = inbounds.get(e.target);
                Iterator<Edge<N>> j = ins.iterator();
                boolean traverse = true;
                while (j.hasNext()) {
                    Edge<N> in = j.next();
                    if (subgraph.contains(in.source) &&
                            !traversed.contains(in.source) &&
                            !node.equals(in.source)) {
                        traverse = false;
                        break;
                    }
                }
                if (traverse) {
                    listener.traverse(e.target);
                    traversed.add(e.target);
                    traverse(e.target, listener, traversed, subgraph);
                }
            }
        }
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        Iterator i = edges.iterator();
        while (i.hasNext()) {
            sb.append(i.next());
            sb.append("\n");
        }
        return sb.toString();
    }
}
