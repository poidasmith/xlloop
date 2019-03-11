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

import static org.boris.expr.parser.ExprTokenType.CloseBrace;
import static org.boris.expr.parser.ExprTokenType.Comma;
import static org.boris.expr.parser.ExprTokenType.Decimal;
import static org.boris.expr.parser.ExprTokenType.Integer;
import static org.boris.expr.parser.ExprTokenType.Multiply;
import static org.boris.expr.parser.ExprTokenType.OpenBrace;
import static org.boris.expr.parser.ExprTokenType.Power;
import static org.boris.expr.parser.ExprTokenType.SemiColon;
import static org.boris.expr.parser.ExprTokenType.String;
import static org.boris.expr.parser.ExprTokenType.StringConcat;
import static org.boris.expr.parser.ExprTokenType.Variable;

import java.io.IOException;

import junit.framework.TestCase;

import org.boris.expr.parser.ExprLexer;
import org.boris.expr.parser.ExprTokenType;

public class LexerTest extends TestCase
{
    public void testString() throws Exception {
        assertTypes("\"A\"&\"B\"&\"C\"", String, StringConcat, String,
                StringConcat, String);
    }

    public void testDecimal() throws Exception {
        assertTypes("23.", Decimal);
        assertTypes("23.*0", Decimal, Multiply, Integer);
    }

    public void testQuotedReferences() throws Exception {
        assertTypes("'Quotes Needed Here &#$@'!A1", Variable);
    }

    public void testOperators() throws Exception {
        assertTypes("23^2", Integer, Power, Integer);
    }

    public void testArrays() throws Exception {
        assertTypes("{12,34}", OpenBrace, Integer, Comma, Integer, CloseBrace);
        assertTypes("{12;34}", OpenBrace, Integer, SemiColon, Integer,
                CloseBrace);
    }

    private void assertTypes(String expr, ExprTokenType... types)
            throws IOException {
        ExprLexer l = new ExprLexer(expr);
        for (int i = 0; i < types.length; i++) {
            assertEquals(l.next().type, types[i]);
        }
        assertNull(l.next());
    }
}
