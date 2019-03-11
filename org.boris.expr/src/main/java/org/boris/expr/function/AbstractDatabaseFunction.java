/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.expr.function;

import org.boris.expr.Expr;
import org.boris.expr.ExprArray;
import org.boris.expr.ExprError;
import org.boris.expr.ExprException;
import org.boris.expr.ExprInteger;
import org.boris.expr.ExprString;
import org.boris.expr.util.Criteria;
import org.boris.expr.util.Database;

public abstract class AbstractDatabaseFunction extends AbstractFunction
{
    public final Expr evaluate(Expr[] args) throws ExprException {
        assertArgCount(args, 3);

        // Get database argument
        Expr edb = evalArg(args[0]);
        if (!(edb instanceof ExprArray)) {
            return ExprError.VALUE;
        }
        Database db = Database.valueOf((ExprArray) edb);
        if (db == null) {
            return ExprError.VALUE;
        }

        // Get field argument
        Expr ef = evalArg(args[1]);
        String field = null;
        if (ef instanceof ExprString) {
            field = ((ExprString) ef).str;
        } else if (ef instanceof ExprInteger) {
            int col = ((ExprInteger) ef).intValue();
            int cc = db.getColumnCount();
            if (col < 1 || col > cc)
                return ExprError.VALUE;
            field = db.getColumnName(col - 1);
        }

        // Get criteria argument
        Expr ec = evalArg(args[2]);
        if (!(ec instanceof ExprArray)) {
            return ExprError.VALUE;
        }
        Criteria criteria = Criteria.valueOf((ExprArray) ec);

        return evaluate(db, field, criteria);
    }

    protected abstract Expr evaluate(Database db, String field,
            Criteria criteria) throws ExprException;
}
