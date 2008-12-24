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
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import org.boris.expr.Expr;
import org.boris.expr.ExprEvaluatable;
import org.boris.expr.ExprException;
import org.boris.expr.ExprFunction;
import org.boris.expr.ExprMissing;
import org.boris.expr.ExprVariable;
import org.boris.expr.IEvaluationCallback;
import org.boris.expr.parser.ExprLexer;
import org.boris.expr.parser.ExprParser;
import org.boris.expr.parser.IParserVisitor;
import org.boris.expr.util.Exprs;
import org.boris.variant.util.Edge;
import org.boris.variant.util.Graph;
import org.boris.variant.util.GraphCycleException;
import org.boris.variant.util.GraphTraversalListener;

public class Engine implements IParserVisitor, IEvaluationCallback,
        GraphTraversalListener
{
    private EngineProvider provider;
    private GridMap inputs = new GridMap();
    private GridMap values = new GridMap();
    private Map<String, Range> aliases = new TreeMap();
    private Graph graph = new Graph();
    private ExprMissing MISSING = new ExprMissing();
    private boolean autoCalculate = true;
    private String namespace;

    public Engine(EngineProvider provider) {
        this.provider = provider;
        this.graph.setIncludeEdges(false);
    }

    public EngineProvider getProvider() {
        return provider;
    }

    public void setAutoCalculate(boolean auto) {
        this.autoCalculate = auto;
    }

    public void setNamespace(String namespace) {
        this.namespace = namespace;
    }

    public void calculate(boolean force) throws ExprException {
        if (autoCalculate && !force)
            return;

        graph.sort();
        Iterator i = graph.iterator();
        while (i.hasNext()) {
            Range r = (Range) i.next();
            Expr input = inputs.get(r);
            if (input instanceof ExprEvaluatable) {
                Expr eval = ((ExprEvaluatable) input).evaluate();
                provider.valueChanged(r, eval);
                values.put(r, eval);
            }
        }
    }

    public void set(String name, String expression) throws ExprException {
        set(Range.valueOf(name), expression);
    }

    public void set(Range range, String expression) throws ExprException {
        validateRange(range);

        // If null then remove all references
        if (expression == null) {
            values.remove(range);
            inputs.remove(range);
            updateDependencies(range, null);
            return;
        }

        Expr expr = parseExpression(expression);

        // Update the dependency graph
        updateDependencies(range, expr);

        // Set the inputs
        provider.inputChanged(range, expr);
        inputs.put(range, expr);

        // Always evaluate the expression entered
        if (expr.evaluatable) {
            Expr eval = ((ExprEvaluatable) expr).evaluate();
            provider.valueChanged(range, eval);
            values.put(range, eval);
        } else {
            provider.valueChanged(range, expr);
            values.put(range, expr);
        }

        // Recalculate the dependencies if required
        if (autoCalculate) {
            graph.traverse(range, this);
        }
    }

    private void updateDependencies(Range range, Expr expr)
            throws ExprException {
        graph.clearInbounds(range);
        ExprVariable[] vars = ExprVariable.findVariables(expr);
        for (ExprVariable var : vars) {
            Range source = (Range) var.getAnnotation();
            try {
                addDependencies(source, range);
            } catch (GraphCycleException ex) {
                for (ExprVariable v : vars) {
                    removeDependencies((Range) v.getAnnotation(), range);
                }
                throw new ExprException(ex);
            }
        }
    }

    private void addDependencies(Range source, Range target)
            throws GraphCycleException {
        if (source.isArray()) {
            Range[] r = source.split();
            for (Range rs : r) {
                graph.add(new Edge(rs, target));
            }
        } else {
            graph.add(new Edge(source, target));
        }
    }

    private void removeDependencies(Range source, Range target) {
        if (source.isArray()) {
            Range[] r = source.split();
            for (Range rs : r) {
                graph.remove(new Edge(rs, target));
            }
        } else {
            graph.remove(new Edge(source, target));
        }
    }

    private void updateAliasedRange(Range range) throws ExprException {
        if (range == null)
            return;

        Range dim1A = getAlias(range.getDimension1Name());
        if (dim1A != null)
            range.setDimension1(dim1A.getDimension1());

        Range dim2A = getAlias(range.getDimension2Name());
        if (dim2A != null) {
            range.setDimension2(range.getDimension1());
        }
    }

    private void validateRange(Range range) throws ExprException {
        updateAliasedRange(range);
        GridReference dim1 = range.getDimension1();
        GridReference dim2 = range.getDimension2();
        int x1 = dim1.getColumn();
        int x2 = dim2 == null ? x1 : dim2.getColumn();
        int y1 = dim1.getRow();
        int y2 = dim2 == null ? y1 : dim2.getRow();
        int width = x2 - x1 + 1;
        int height = y2 - y1 + 1;
        if (width <= 0 || height <= 0)
            throw new ExprException("Invalid range: " + range);
    }

    private Expr parseExpression(String expression) throws ExprException {
        Expr result;
        if (!expression.startsWith("=")) {
            result = Exprs.parseValue(expression);
        } else {
            expression = expression.substring(1);
            ExprParser p = new ExprParser();
            p.setParserVisitor(this);
            try {
                p.parse(new ExprLexer(expression), this);
            } catch (IOException e) {
                throw new ExprException(e);
            }
            result = p.get();
        }
        return result;
    }

    public void addAlias(String name, Range range) {
        if (name != null)
            aliases.put(name.toUpperCase(), range);
    }

    public void removeAlias(String name) {
        if (name != null)
            aliases.remove(name.toUpperCase());
    }

    public Range getAlias(String name) {
        if (name == null)
            return null;
        return aliases.get(name.toUpperCase());
    }

    public void annotateFunction(ExprFunction function) throws ExprException {
    }

    public void annotateVariable(ExprVariable variable) throws ExprException {
        Range r = null;
        try {
            r = Range.valueOf(variable.getName());
            updateAliasedRange(r);
        } catch (ExprException e) {
        }
        variable.setAnnotation(r);
    }

    public Expr evaluateFunction(ExprFunction function) throws ExprException {
        return provider.evaluateFunction(function);
    }

    public Expr evaluateVariable(ExprVariable variable) throws ExprException {
        Range r = (Range) variable.getAnnotation();
        if (r == null) {
            provider.validate(variable);
            return MISSING;
        }

        String ns = r.getNamespace();
        if (ns != null && !ns.equals(namespace)) {
            Expr e = provider.evaluateVariable(variable);
            if (e == null)
                e = MISSING;
            return e;
        } else {

            Expr e = values.get(r);
            if (e == null) {
                // TODO: think about external variables versus missing
                e = provider.evaluateVariable(variable);
                if (e == null)
                    e = MISSING;
            }
            return e;
        }
    }

    public void traverse(Object node) {
        Range r = (Range) node;
        Expr input = inputs.get(r);
        if (input instanceof ExprEvaluatable) {
            try {
                Expr eval = ((ExprEvaluatable) input).evaluate();
                provider.valueChanged(r, eval);
                values.put(r, eval);
            } catch (ExprException e) {
                e.printStackTrace();
                // TODO: handle
            }
        }
    }

    public Expr getInput(Range range) {
        return inputs.get(range);
    }

    public Expr getValue(Range range) {
        return values.get(range);
    }
}
