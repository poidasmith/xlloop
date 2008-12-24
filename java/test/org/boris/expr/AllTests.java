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

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{

    public static Test suite() {
        TestSuite suite = new TestSuite("Test for org.boris.expr");
        //$JUnit-BEGIN$
        suite.addTestSuite(ExprTest.class);
        suite.addTestSuite(ExcelLookupAndReferenceFunctionsTest.class);
        suite.addTestSuite(ExcelCompatTest.class);
        suite.addTestSuite(ParserTest.class);
        suite.addTestSuite(ExcelStatisticalFunctionsTest.class);
        suite.addTestSuite(ExcelMathAndTrigFunctionsTest.class);
        suite.addTestSuite(ExcelDateTest.class);
        suite.addTestSuite(ExprFunctionTest.class);
        suite.addTestSuite(LexerTest.class);
        suite.addTestSuite(ExcelDateAndTimeFunctionsTest.class);
        suite.addTestSuite(ExcelInformationFunctionsTest.class);
        suite.addTestSuite(GridReferenceTest.class);
        suite.addTestSuite(ExcelTextFunctionsTest.class);
        suite.addTestSuite(ExcelDatabaseFunctionsTest.class);
        suite.addTestSuite(RangeTest.class);
        suite.addTestSuite(EngineTest.class);
        suite.addTestSuite(ExcelFinancialFunctionsTest.class);
        suite.addTestSuite(ExcelLogicalFunctionsTest.class);
        //$JUnit-END$
        return suite;
    }

}
