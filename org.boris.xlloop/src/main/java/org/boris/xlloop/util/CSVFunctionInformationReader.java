/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;

import org.boris.xlloop.handler.FunctionInformation;

public class CSVFunctionInformationReader
{
    public static FunctionInformation[] read(InputStream is) throws IOException {
        return read(new InputStreamReader(is));
    }

    public static FunctionInformation[] read(Reader r) throws IOException {
        return read(r, ',', true);
    }

    public static FunctionInformation[] read(Reader r, char delim,
            boolean hasQuotes) throws IOException {
        ArrayList l = new ArrayList();
        BufferedReader br = new BufferedReader(r);
        String line = br.readLine(); // skip first line (assume its a header)
        while ((line = br.readLine()) != null) {
            String[] s = CSV.parseLine(line, delim, hasQuotes);
            l.add(fromArray(s));
        }

        return (FunctionInformation[]) l.toArray(new FunctionInformation[0]);
    }

    public static FunctionInformation fromArray(String[] sa) {
        if (sa == null || sa.length == 0)
            return null;
        FunctionInformation fi = new FunctionInformation(sa[0]);
        if (sa.length > 1)
            fi.setFunctionHelp(sa[1]);
        if (sa.length > 2)
            fi.setCategory(sa[2]);
        if (sa.length > 3)
            fi.setShortcutText(sa[3]);
        if (sa.length > 4)
            fi.setHelpTopic(sa[4]);
        if (sa.length > 5)
            fi.setVolatile(Boolean.valueOf(sa[5]).booleanValue());
        for (int i = 6; i < sa.length; i += 2) {
            String name = sa[i];
            if (name == null || name.trim().length() == 0)
                continue;
            String help = null;
            if (sa.length > i + 1)
                help = sa[i + 1];
            fi.addArgument(name, help);
        }
        return fi;
    }
}
