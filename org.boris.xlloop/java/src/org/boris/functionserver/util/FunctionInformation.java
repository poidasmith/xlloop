package org.boris.functionserver.util;

import java.util.ArrayList;
import java.util.List;

import org.boris.variantcodec.VTCollection;
import org.boris.variantcodec.VTStruct;

public class FunctionInformation
{
    private String functionName;
    private String functionText;
    private String functionHelp;
    private String category;
    private String macroType;
    private List arguments = new ArrayList();
    private List argumentHelp = new ArrayList();

    public FunctionInformation(String name) {
        this.functionName = name;
    }

    public void setFunctionText(String functionText) {
        this.functionText = functionText;
    }

    public void setFunctionHelp(String functionHelp) {
        this.functionHelp = functionHelp;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public void setMacroType(String macroType) {
        this.macroType = macroType;
    }

    public void addArgument(String name, String help) {
        this.arguments.add(name);
        this.argumentHelp.add(help);
    }

    public VTStruct encode() {
        VTStruct s = new VTStruct();
        s.add("functionName", functionName);
        if (functionText != null)
            s.add("functionText", functionText);
        if (functionHelp != null)
            s.add("functionHelp", functionHelp);
        if (category != null)
            s.add("category", category);
        if (macroType != null)
            s.add("macroType", macroType);
        if (arguments.size() > 0) {
            s.add("argumentText", CSV.toCSV((String[]) arguments.toArray(new String[0])));
            VTCollection c = new VTCollection();
            for (int i = 0; i < argumentHelp.size(); i++) {
                c.add((String) argumentHelp.get(i));
            }
            s.add("argumentHelp", c);
        }
        return s;
    }
}
