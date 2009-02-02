/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop;

import java.io.InputStreamReader;

import org.boris.xlloop.handler.FunctionInformation;
import org.boris.xlloop.util.CSVFunctionInformationReader;

public class CSVFITester
{
    public static void main(String[] args) throws Exception {
        FunctionInformation[] fi = CSVFunctionInformationReader
                .read(new InputStreamReader(CSVFITester.class
                        .getResourceAsStream("function_info1.csv")));
        for (int i = 0; i < fi.length; i++) {
            System.out.println(fi[i]);
        }
    }
}
