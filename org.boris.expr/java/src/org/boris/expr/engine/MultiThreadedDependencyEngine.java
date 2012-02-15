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
import java.util.concurrent.atomic.AtomicBoolean;

import org.boris.expr.Expr;
import org.boris.expr.ExprError;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprFunction;

public class MultiThreadedDependencyEngine
{
    private Map<String, Node> nodes = new HashMap<String, Node>();
    private Set<IEngineListener> listeners = new HashSet<IEngineListener>();
    
    private void set(String name, Expr input) {
        Node n = nodes.get(name);

        // Check existing node and remove it (and all child nodes)
        if(n != null) {
            n.remove();
        }
        
        // recurse down Expr tree creating child nodes
        Expr[] args = input.getArgs();
        if(args != null) {
            for(int i = 0; i < args.length; i++) {
                set(name + "/" + i, args[i]);
            }
        }
        
        // For each child we add ourself to 'after' Set
    }
    
    public class FunctionNode extends Node {
        private ExprFunction function;
        private Node[] args;

        public FunctionNode(ExprFunction function, Node[] args) {
            this.function = function;
            this.args = args;
        }
    }
    
    public class Node  {
        /**
         * Used to identify ourselves within the map.
         */
        private String name;
        
        /**
         * The set of nodes that this node depends on.
         */
        private Set<Node> before = new HashSet<Node>(); 
        
        /** 
         * The set of nodes that are nominally children of this node.
         * For example the nested arguments of a function.
         * This is a strict subset of before. 
         */
        private Set<Node> children = new HashSet<Node>();
        
        /**
         * The set of nodes that depend on this node.
         * TBD: do we actually need this?
         * probably not as we won't be walking.
         */
        private Set<Node> after = new HashSet<Node>();
        
        /**
         * The input evaluatable.
         * Each argument points to a child node (or value)
         * We will wrap each arg in a delegate that
         */
        private ExprEvaluatable input;
        
        /**
         * The result of our evaluation (or the input value)
         */
        private Expr result;
        
        private AtomicBoolean dirtyFlag = new AtomicBoolean(true);
        
        /**
         * Walk the tree to mark every downstream node as dirty.
         */
        public void markAsDirty() {
            dirtyFlag.set(true);
            for(Node n : after)
                n.markAsDirty();
        }
        
        /**
         * Remove this and all child nodes from the graph. 
         */
        public void remove() {
            nodes.remove(this);
            for(Node n : children)
                n.remove();
        }

        /**
         * TODO: need to synchronize this with 'modifiers'.
         */
        private boolean isBeforeClean() {
            for(Node n : before)
                if(n.dirtyFlag.get())
                    return false;
            return true;
        }

        /**
         * Main input to evaluation thread.
         * We could flip between this and checkIsClean.
         * Could be weighted so for a calc thread we run
         * through 5 evaluate loops and 1 checkIsClean loop.
         */
        public Expr evaluate() {
            if(dirtyFlag.get()) {
                if(isBeforeClean()) {
                    fireBeforeCalculation(name);
                    try {
                        result = input.evaluate();
                    } catch (ExprException e) {
                        result = new ExprError(e);
                    }
                    fireAfterCalculation(name, result);
                    dirtyFlag.set(false);
                }
            }
            return result;
        }
        
        public int hashCode() {
            return name.hashCode();
        }
        
        public boolean equals(Object obj) {
            return ((Node)obj).name.equals(name);
        }
    }
    
    private void fireBeforeCalculation(String name) {
        for(IEngineListener listener : listeners)
            listener.beforeCalculation(name);
    }

    private void fireAfterCalculation(String name, Expr result) {
        for(IEngineListener listener : listeners)
            listener.afterCalculation(name, result);
    }

    private class NodeWorker implements Runnable {
        private long calcPause = 1000;
        private boolean running = false;
        private Set<Node> nodes = new HashSet<Node>();

        public void run() {
            while(running) {
                for(Node n : nodes)
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
