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

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.boris.expr.Expr;
import org.boris.expr.ExprError;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprFunction;
import org.boris.expr.ExprVariable;
import org.boris.expr.IEvaluationCallback;
import org.boris.expr.function.IFunctionProvider;
import org.boris.expr.parser.ExprParser;

public class ListenerEngine implements IEvaluationCallback
{
    private IFunctionProvider functionProvider;
    private Map<String, Node> nodes = new HashMap<String, Node>();
    private Set<IEngineListener> listeners = new HashSet<IEngineListener>();
    private Queue<Node> calculationQueue = new LinkedBlockingQueue<Node>();
    private NodeWorker[] workers;
    
    public ListenerEngine(IFunctionProvider funcs) {
        this.functionProvider = funcs;
    }

    public Expr evaluateFunction(ExprFunction function) throws ExprException {
        return functionProvider.evaluate(function);
    }

    public Expr evaluateVariable(ExprVariable variable) throws ExprException {
        Node n = nodes.get(variable.getName());
        return n == null ? ExprError.NAME : n.result;
    }
    
    public void startCalculation(ExecutorService calculationPool, int numWorkers) {
        if(workers != null) {
            stopCalculation();
        }
        workers = new NodeWorker[numWorkers];
        for(int i =0 ; i < workers.length; i++) {
            workers[i] = new NodeWorker();
            calculationPool.execute(workers[i]);
        }
    }
    
    public void stopCalculation() {
        if(workers != null) {
            for(NodeWorker nw : workers)
                nw.setStopped();
            workers = null;
        }
    }
    
    public void addListener(IEngineListener listener) {
        listeners.add(listener);
    }
    
    public void removeListener(IEngineListener listener) {
        listeners.remove(listener);
    }

    public void set(String name, String expression) throws IOException, ExprException {
        Expr input = ExprParser.parse(expression, this);
        set(name, input);
    }
    
    public void set(String name, Expr input) {
        Node n = nodes.get(name);
        if (n == null) {
            n = new Node(name);
        }
        n.setInput(input == null ? ExprError.NULL : input);
    }

    public Expr get(String name) {
        Node n = nodes.get(name);
        return n == null ? null : n.result;
    }

    public class Node
    {
        private String name;
        private Set<Node> before = new HashSet<Node>();
        private Set<Node> after = new HashSet<Node>();
        private AtomicBoolean dirtyFlag = new AtomicBoolean(false);
        private Expr input;
        private Expr result;

        public Node(String name) {
            this.name = name;
            setInput(ExprError.REF);
        }

        public Node(String name, Expr input) {
            this.name = name;
            setInput(input);
        }

        public void setInput(Expr input) {
            if (this.input != null) {
                removeDependencies();
            }
            this.input = input;
            setDirty();
            addDependencies();
        }

        public boolean isDirty() {
            return dirtyFlag.get();
        }

        public Expr evaluate() {
            if (dirtyFlag.get()) {
                for (IEngineListener l : listeners)
                    l.beforeCalculation(name);
                if (input instanceof ExprEvaluatable) {
                    try {
                        result = ((ExprEvaluatable) input).evaluate();
                    } catch (ExprException e) {
                        result = new ExprError(e);
                    }
                } else {
                    result = input;
                }
                dirtyFlag.set(false);
                for (Node n : after)
                    n.setDirty();
                for (IEngineListener l : listeners)
                    l.afterCalculation(name, result);
            }

            return result;
        }

        private void setDirty() {
            dirtyFlag.set(true);
            checkReadyToCalc();
        }
        
        private void checkReadyToCalc() {
            if(!dirtyFlag.get())
                return;
            // If all of our dependencies are clean we are ready to calc
            for (Node n : before)
                if (n.isDirty())
                    return;
            calculationQueue.add(this);
        }

        private void addDependencies() {
            ExprVariable[] depends = ExprVariable.findVariables(input);

            for (ExprVariable var : depends) {
                String name = var.getName();
                Node n = nodes.get(name);
                if (n == null) {
                    nodes.put(name, n = new Node(name));
                }
                before.add(n);
                n.after.add(this);
            }
        }

        private void removeDependencies() {
            before.clear();

            for (Node n : before) {
                n.after.remove(this);
            }
        }
    }

    public class NodeWorker implements Runnable
    {
        private AtomicBoolean stopped = new AtomicBoolean(false);
        private AtomicLong pause = new AtomicLong(100);

        public void run() {
            while (!stopped.get()) {
                Node n = calculationQueue.poll();
                if (n != null) {
                    n.evaluate();
                }
                try {
                    Thread.sleep(pause.get());
                } catch (InterruptedException e) {
                    break;
                }
            }
        }

        public void setCalculationPause(long pause) {
            this.pause.set(pause);
        }

        public void setStopped() {
            stopped.set(true);
        }
    }
    
    public class NodeCleanChecker implements Runnable 
    {
        private AtomicBoolean stopped = new AtomicBoolean(true);
        private AtomicLong pause = new AtomicLong(10000);

        public void run() {
            while (!stopped.get()) {
                for(Node n : nodes.values()) {
                    n.checkReadyToCalc();
                }
                try {
                    Thread.sleep(pause.get());
                } catch (InterruptedException e) {
                    break;
                }
            }
        }

        public void setCalculationPause(long pause) {
            this.pause.set(pause);
        }

        public void setStopped() {
            stopped.set(true);
        }
    }
}
