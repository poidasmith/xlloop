/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.jxll;

public class FunctionInformation
{
    public String procedure;
    public String typeText;
    public String functionName;
    public String argumentText;
    public String macroType;
    public String category;
    public String shortcutText;
    public String helpTopic;
    public String functionHelp;
    public String[] argumentHelp;

    // Derived
    public FunctionSpec type;

    public FunctionInformation() {
    }

    public FunctionInformation(String name, String type) {
        this.functionName = name;
        this.procedure = name;
        this.typeText = type;
        this.type = FunctionSpec.valueOf(type);
    }
}
