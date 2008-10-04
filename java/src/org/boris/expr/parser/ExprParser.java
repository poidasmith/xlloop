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
import org.boris.expr.ExprDouble;
import org.boris.expr.ExprException;
import org.boris.expr.ExprInteger;
import org.boris.expr.ExprString;
import org.boris.expr.ExprSubtraction;
import org.boris.expr.ExprVariable;
import org.boris.expr.IBinaryOperator;
import org.boris.expr.IEvaluationCallback;

public class ExprParser
{
    private Expr current;

    public void parse(ExprLexer lexer, IEvaluationCallback callback)
            throws IOException, ExprException {
        ExprToken e = null;
        while ((e = lexer.next()) != null) {
            switch (e.type) {
            case Plus:
            case Minus:
            case Multiply:
            case Divide:
            case StringConcat:
                parseOperator(e);
                break;
            case Decimal:
            case Integer:
            case String:
            case Variable:
                parseValue(e, callback);
                break;
            case OpenBracket:
                parseExpression(lexer, callback);
                break;
            case Function:
                parseFunction(lexer, callback);
            }
        }
    }

    private void parseFunction(ExprLexer lexer, IEvaluationCallback callback) {
    }

    private void parseExpression(ExprLexer lexer, IEvaluationCallback callback) {
    }

    private void parseValue(ExprToken e, IEvaluationCallback callback)
            throws ExprException {
        Expr value = null;
        switch (e.type) {
        case Decimal:
            value = new ExprDouble(e.doubleValue);
            break;
        case Integer:
            value = new ExprInteger(e.integerValue);
            break;
        case String:
            value = new ExprString(e.val);
            break;
        case Variable:
            value = new ExprVariable(callback, e.val);
            break;
        }
        setValue(value);
    }

    private void setValue(Expr value) throws ExprException {
        if (current == null) {
            current = value;
            return;
        } else {
            Expr c = current;
            do {
                if (!(c instanceof IBinaryOperator))
                    throw new ExprException("Expected operator not found");

                Expr rhs = ((IBinaryOperator) c).getRHS();
                if (rhs == null) {
                    ((IBinaryOperator) c).setRHS(value);
                    return;
                } else {
                    c = rhs;
                }
            } while (c != null);

            throw new ExprException("Unexpected token found");
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
