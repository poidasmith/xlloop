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

public class ExprToken
{
    public static final ExprToken OPEN_BRACKET = new ExprToken(
            ExprTokenType.OpenBracket, "(");
    public static final ExprToken CLOSE_BRACKET = new ExprToken(
            ExprTokenType.CloseBracket, ")");
    public static final ExprToken PLUS = new ExprToken(ExprTokenType.Plus, "+");
    public static final ExprToken MINUS = new ExprToken(ExprTokenType.Minus,
            "-");
    public static final ExprToken MULTIPLY = new ExprToken(
            ExprTokenType.Multiply, "*");
    public static final ExprToken DIVIDE = new ExprToken(ExprTokenType.Divide,
            "/");
    public static final ExprToken COMMA = new ExprToken(ExprTokenType.Comma,
            ",");
    public static final ExprToken STRING_CONCAT = new ExprToken(
            ExprTokenType.StringConcat, "&");

    public final ExprTokenType type;
    public final String val;
    public final double doubleValue;
    public final int integerValue;

    public ExprToken(ExprTokenType type, String val) {
        this.type = type;
        this.val = val;
        this.doubleValue = 0.;
        this.integerValue = 0;
    }

    public ExprToken(String val, double doubleValue) {
        this.type = ExprTokenType.Decimal;
        this.val = val;
        this.doubleValue = doubleValue;
        this.integerValue = 0;
    }

    public ExprToken(String val, int integerValue) {
        this.type = ExprTokenType.Integer;
        this.val = val;
        this.doubleValue = 0.;
        this.integerValue = integerValue;
    }

    public String toString() {
        return type.toString() + ":" + val;
    }
}
