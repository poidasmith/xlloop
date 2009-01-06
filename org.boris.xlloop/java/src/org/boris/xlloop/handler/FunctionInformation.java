/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.handler;

import java.util.ArrayList;
import java.util.List;

import org.boris.xlloop.util.CSV;
import org.boris.xlloop.util.XLList;
import org.boris.xlloop.util.XLMap;
import org.boris.xlloop.xloper.XLoper;

public class FunctionInformation
{
    private String functionName;
    private String functionHelp;
    private String category;
    private String shortcutText;
    private String helpTopic;
    private List arguments = new ArrayList();
    private List argumentHelp = new ArrayList();

    public FunctionInformation(String name) {
        this.functionName = name;
    }

    public void setFunctionHelp(String functionHelp) {
        this.functionHelp = functionHelp;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public void setShortcutText(String shortcutText) {
        this.shortcutText = shortcutText;
    }

    public void setHelpTopic(String helpTopic) {
        this.helpTopic = helpTopic;
    }

    public void addArgument(String name, String help) {
        this.arguments.add(name);
        this.argumentHelp.add(help);
    }

    public XLoper encode() {
        XLMap s = new XLMap();
        s.add("functionName", functionName);
        if (functionHelp != null)
            s.add("functionHelp", functionHelp);
        if (category != null)
            s.add("category", category);
        if (shortcutText != null)
            s.add("shortcutText", shortcutText);
        if (helpTopic != null)
            s.add("helpTopic", helpTopic);
        if (arguments.size() > 0) {
            s.add("argumentText", CSV.toCSV((String[]) arguments
                    .toArray(new String[0])));
            XLList c = new XLList();
            for (int i = 0; i < argumentHelp.size(); i++) {
                c.add((String) argumentHelp.get(i));
            }
            s.add("argumentHelp", c);
        }
        return s.toXloper();
    }
}
