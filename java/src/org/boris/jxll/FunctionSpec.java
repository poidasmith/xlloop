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

public class FunctionSpec
{
    public final boolean isVolatile;
    public final int[] types;
    public final int returnType;

    private FunctionSpec(int[] types, boolean isVolatile) {
        this.returnType = types[0];
        this.types = new int[types.length - 1];
        System.arraycopy(types, 1, this.types, 0, types.length - 1);
        this.isVolatile = isVolatile;
    }

    public static FunctionSpec valueOf(String s) {
        if (s == null)
            return null;
        int len = s.length();
        if (len < 1)
            return null;
        boolean isVolatile = false;
        if (s.endsWith("!")) {
            isVolatile = true;
            s = s.substring(0, len - 1);
            len--;
        }
        int[] types = new int[len];
        for (int i = 0; i < len; i++) {
            char c = s.charAt(i);
            switch (c) {
            case 'A':
                types[i] = ArgType.argBoolean;
                break;
            case 'L':
                types[i] = ArgType.argBooleanPtr;
                break;
            case 'B':
                types[i] = ArgType.argDouble;
                break;
            case 'E':
                types[i] = ArgType.argDoublePtr;
                break;
            case 'C':
            case 'F':
                types[i] = ArgType.argString;
                break;
            case 'D':
            case 'G':
                types[i] = ArgType.argStringLC;
                break;
            case 'H':
                types[i] = ArgType.argUShort;
                break;
            case 'I':
                types[i] = ArgType.argShort;
                break;
            case 'M':
                types[i] = ArgType.argShortPtr;
                break;
            case 'J':
                types[i] = ArgType.argInt;
                break;
            case 'N':
                types[i] = ArgType.argIntPtr;
                break;
            case 'P':
                types[i] = ArgType.argXLOper;
                break;
            case 'R':
                types[i] = ArgType.argXLOperRef;
                break;
            default:
                // Unknown arg type/invalid type text
                return null;
            }
        }

        return new FunctionSpec(types, isVolatile);
    }
}
