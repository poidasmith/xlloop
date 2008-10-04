/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr.parser;

import java.io.IOException;

import org.boris.expr.Expr;
import org.boris.expr.ExprAddition;
import org.boris.expr.ExprSubtraction;
import org.boris.expr.IEvaluationCallback;

public class ExprParser
{
    private Expr current;

    public void parse(ExprLexer lexer, IEvaluationCallback callback)
            throws IOException {
        ExprToken e = null;
        while ((e = lexer.next()) != null) {
            switch (e.type) {
            case Plus:
            case Minus:
            case Multiply:
            case Divide:
                parseOperator(e);
                break;
            }
        }
    }

    private void parseOperator(ExprToken e) {
        switch (e.type) {
        case Plus:
            Expr lhs = current;
            current = new ExprAddition(lhs, null);
            break;
        case Minus:
            lhs = current;
            current = new ExprSubtraction(lhs, null);
            break;
        case Multiply:
            break;
        case Divide:
            break;
        }
    }

    public Expr get() {
        return current;
    }
}
