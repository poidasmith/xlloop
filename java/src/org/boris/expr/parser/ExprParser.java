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

public class ExprParser
{
    public void parse(ExprLexer lexer) throws IOException {
        ExprToken e = null;
        while ((e = lexer.next()) != null) {
            parse(e);
        }
    }

    private void parse(ExprToken e) {
    }

    public Expr get() {
        return null;
    }
}
