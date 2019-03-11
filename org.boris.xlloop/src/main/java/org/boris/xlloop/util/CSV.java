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
import java.io.File;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;

public class CSV
{
    public static String toCSV(String[] arr) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < arr.length; i++) {
            sb.append(arr[i]);
            if (i < arr.length - 1)
                sb.append(",");
        }
        return sb.toString();
    }

    /**
     * Parse a line.
     * 
     * @param line.
     * 
     * @return String[].
     */
    public static String[] parseLine(String line, char delim, boolean hasQuotes) {
        ArrayList items = new ArrayList();
        StringBuilder sb = new StringBuilder();
        boolean inQuote = false;
        int length = line.length();

        for (int i = 0; i < length; i++) {
            char c = line.charAt(i);

            if ((c == delim) && !inQuote) {
                items.add(sb.toString());
                sb.setLength(0);
            } else if (hasQuotes && (c == '\"')) {
                inQuote = !inQuote;
            } else {
                sb.append(c);
            }
        }

        items.add(sb.toString());

        return (String[]) items.toArray(new String[0]);
    }

    /**
     * Try to open the given url as a file or url.
     * 
     * @param url.
     * 
     * @return BufferedReader.
     */
    public static BufferedReader openUrl(String url) throws Exception {
        // Try a url first
        try {
            URL u = new URL(url);

            return new BufferedReader(new InputStreamReader(u.openStream()));
        } catch (Exception ex) {
            File f = new File(url);

            if (f.exists()) {
                return new BufferedReader(new FileReader(f));
            }
        }

        return null;
    }

    /**
     * Read a CSV from a file or URL.
     * 
     * @param url.
     * 
     * @return String[][] (as the csv array).
     */
    public static String[][] readEx(String url, String delim, boolean hasQuotes)
            throws Exception {
        BufferedReader br = CSV.openUrl(url);

        if (br == null) {
            return new String[][] { { "Could not open: " + url } };
        }

        ArrayList lines = new ArrayList();
        String line = null;

        while ((line = br.readLine()) != null) {
            lines.add(CSV.parseLine(line,
                    (delim != null && delim.length() > 0) ? delim.charAt(0)
                            : ',', hasQuotes));
        }

        br.close();

        return (String[][]) lines.toArray(new String[][] {});
    }
}
