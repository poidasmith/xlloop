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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Executor;
import java.util.concurrent.atomic.AtomicBoolean;

import org.boris.expr.Expr;
import org.boris.expr.ExprError;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprFunction;
import org.boris.expr.ExprVariable;

public class MultiThreadedDependencyEngine
{
    private Map<String, Node> nodes = new HashMap<String, Node>();
    private Map<String, Expr> variables = new HashMap<String, Expr>();
    private Executor[] executors;
    private NodeWorker[] workers;
    private Set<IEngineListener> listeners = new HashSet<IEngineListener>();
    
    /**
     * Set the calc region - the NodeWorkers will only calc Nodes within this region
     *   (or vars)
     * 
     * This is useful in the case of a large sheet that the user scrolls around.
     * We only need/want to calculate vars associated with the visible area. 
     *  (this could be a sheet option)  
     *   
     * We need to think about region-specific vars. Ie vars that are calculated only
     * within a region.
     */
    private Range currentCalcRegion;

    public MultiThreadedDependencyEngine(Executor[] executors) {
        this.executors = executors;
        this.workers = new NodeWorker[executors.length];
    }

    public void set(String name, Expr input) {
        variables.remove(name);
        Node n = nodes.get(name);

        // Check existing node and remove it (and all child nodes)
        if (n != null) {
            n.remove();
        }

        // Create evaluator

        // For each child we add ourself to 'after' Set

        // Our ourself to the least loaded worker
        nodes.put(name, n);
        int workerIndex = -1;
        int nodeCount = Integer.MAX_VALUE;
        for (int i = 0; i < workers.length; i++) {
            int sz = workers[i].nodes.size();
            if (sz < nodeCount) {
                workerIndex = i;
                nodeCount = sz;
            }
        }
        if (workerIndex != -1)
            workers[workerIndex].nodes.add(n); // TODO: synchronized with worker
                                               // thread
    }

    public Expr get(String name) {
        Node n = nodes.get(name);
        return n == null ? variables.get(name) : n.result;
    }

    public Set<String> nodeSet() {
        return nodes.keySet();
    }

    public class Node
    {
        /**
         * Used to identify ourselves within the map.
         */
        protected String name;

        /**
         * The set of nodes that this node depends on.
         */
        protected Set<Node> before = new HashSet<Node>();

        /**
         * The set of nodes that are nominally children of this node. For
         * example the nested arguments of a function. This is a strict subset
         * of before.
         */
        protected Set<Node> children = new HashSet<Node>();

        /**
         * The set of nodes that depend on this node. TBD: do we actually need
         * this? probably not as we won't be walking.
         */
        protected Set<Node> after = new HashSet<Node>();

        /**
         * Responsible for evaluating this node. 
         * Can be replaced as node is created/modified. 
         */
        protected NodeEvaluation evalautor;
        
        /**
         * The result of our evaluation (or the input value)
         */
        protected Expr result;
        
        /**
         * Indicates that the result value is dirty.
         */
        protected AtomicBoolean dirtyFlag = new AtomicBoolean(true);
        
        /**
         * Walk the tree to mark every downstream node as dirty.
         */
        public void markAsDirty() {
            if(!dirtyFlag.get()) {
                dirtyFlag.set(true);
                for (Node n : after)
                    n.markAsDirty();
            }
        }

        /**
         * Remove this and all child nodes from the graph.
         */
        public void remove() {
            nodes.remove(this);
            for (Node n : children)
                n.remove();
        }

        /**
         * TODO: need to synchronize this with 'modifiers'.
         */
        private boolean isBeforeClean() {
            for (Node n : before)
                if (n.dirtyFlag.get())
                    return false;
            return true;
        }

        /**
         * Main input to evaluation thread. We could flip between this and
         * checkIsClean. Could be weighted so for a calc thread we run through 5
         * evaluate loops and 1 checkIsClean loop.
         */
        public Expr evaluate() {
            if (dirtyFlag.get()) {
                if (isBeforeClean()) {
                    fireBeforeCalculation(name);
                    try {
                        evalautor.evaluate();
                    } catch (ExprException e) {
                        result = new ExprError(e);
                    }
                    fireAfterCalculation(name, result);
                    dirtyFlag.set(false);
                }
            }
            return result;
        }

        /* (non-Javadoc)
         * @see java.lang.Object#hashCode()
         */
        public int hashCode() {
            return name.hashCode();
        }

        /* (non-Javadoc)
         * @see java.lang.Object#equals(java.lang.Object)
         */
        public boolean equals(Object obj) {
            return ((Node) obj).name.equals(name);
        }
    }
    
    private class NodeEvaluation {
        /**
         * The input evaluatable. Each argument points to a child node (or
         * value) We will wrap each arg in a delegate that
         */
        protected ExprEvaluatable input;

        protected Expr evaluate() throws ExprException {
            return input.evaluate();
        }        
    }

    private void fireBeforeCalculation(String name) {
        for (IEngineListener listener : listeners)
            listener.beforeCalculation(name);
    }

    private void fireAfterCalculation(String name, Expr result) {
        for (IEngineListener listener : listeners)
            listener.afterCalculation(name, result);
    }

    private class NodeWorker implements Runnable
    {
        private long calcPause = 1000;
        private boolean running = false;
        private Set<Node> nodes = new HashSet<Node>(); // TODO: synchronized
                                                       // with 'modifiers'

        public void run() {
            while (running) {
                for (Node n : nodes)
                    n.evaluate();
                try {
                    Thread.sleep(calcPause);
                } catch (InterruptedException e) {
                    // TODO: something here
                }
            }
        }
    }
}
