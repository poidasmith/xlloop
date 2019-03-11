/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.reflect;

import java.io.DataInputStream;
import java.io.IOException;
import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Parses class file to extract parameter names if available.
 */
public class ParameterNameExtractor
{
    private Class clazz;
    private ConstantInfo[] constants;
    private int[] interfaces;
    private FieldInfo[] fields;
    private MethodInfo[] methods = new MethodInfo[0];
    private Attribute[] attributes;

    public ParameterNameExtractor(Class clazz) {
        this.clazz = clazz;
        try {
            parse();
        } catch (IOException e) {
        }
    }

    public String[] getParameterNames(AccessibleObject methodOrCtor) {
        String methodName = null;
        Class returnType = null;
        Class[] params = null;
        boolean isStatic = false;
        if (methodOrCtor instanceof Method) {
            methodName = ((Method) methodOrCtor).getName();
            returnType = ((Method) methodOrCtor).getReturnType();
            params = ((Method) methodOrCtor).getParameterTypes();
            isStatic = Modifier.isStatic(((Method) methodOrCtor).getModifiers());
        } else if (methodOrCtor instanceof Constructor) {
            methodName = "<init>";
            returnType = Void.TYPE;
            params = ((Constructor) methodOrCtor).getParameterTypes();
        } else {
            return null;
        }

        int paramOffset = isStatic ? 0 : 1;
        ArrayList paramNames = new ArrayList();
        boolean found = false;
        for (int i = 0; i < methods.length; i++) {
            String name = constants[methods[i].nameIndex - 1].string;
            Attribute[] attrs = methods[i].attributes;
            if (methodName.equals(name)) {
                String descriptor = constants[methods[i].descriptorIndex - 1].string;
                if (descriptor.equals(generateDescriptor(returnType, params))) {
                    for (int j = 0; j < attrs.length; j++) {
                        if (attrs[j] instanceof CodeAttribute) {
                            Attribute[] cas = ((CodeAttribute) attrs[j]).attributes;
                            for (int k = 0; k < cas.length; k++) {
                                if (cas[k] instanceof LocalVariableTableAttribute) {
                                    LocalVariableTable[] lvt = ((LocalVariableTableAttribute) cas[k]).localVariableTable;
                                    for (int l = 0; l < params.length; l++) {
                                        paramNames.add(constants[lvt[l + paramOffset].nameIndex - 1].string);
                                    }
                                    found = true;
                                }
                            }
                        }
                    }
                }
            }
        }

        // Just stick in default param names
        if (!found) {
            for (int i = 0; i < params.length; i++) {
                paramNames.add("arg" + i);
            }
        }

        return (String[]) paramNames.toArray(new String[0]);
    }

    private static String generateDescriptor(Class returnType, Class[] params) {
        StringBuilder sb = new StringBuilder();
        sb.append("(");
        for (int i = 0; i < params.length; i++) {
            sb.append(generateDescriptor(params[i]));
        }
        sb.append(")");
        sb.append(generateDescriptor(returnType));
        return sb.toString();
    }

    private static Map primitiveDesc = new HashMap();
    static {
        primitiveDesc.put("boolean", "Z");
        primitiveDesc.put("byte", "B");
        primitiveDesc.put("char", "C");
        primitiveDesc.put("short", "S");
        primitiveDesc.put("int", "I");
        primitiveDesc.put("long", "J");
        primitiveDesc.put("float", "F");
        primitiveDesc.put("double", "D");
        primitiveDesc.put("void", "V");
    }

    private static String generateDescriptor(Class type) {
        if (type.isArray()) {
            return "[" + generateDescriptor(type.getComponentType());
        } else if (type.isPrimitive()) {
            return (String) primitiveDesc.get(type.getName());
        } else {
            String name = type.getName();
            name = name.replaceAll("\\.", "/");
            return "L" + name + ";";
        }
    }

    private void parse() throws IOException {
        String name = '/' + clazz.getName().replace('.', '/') + ".class";
        DataInputStream dis = new DataInputStream(ParameterNameExtractor.class.getResourceAsStream(name));
        /* int magic = */dis.readInt();
        /* int minorVersion = */dis.readUnsignedShort();
        /* int majorVersion = */dis.readUnsignedShort();
        constants = new ConstantInfo[dis.readUnsignedShort() - 1];
        for (int i = 0; i < constants.length; i++) {
            constants[i] = parseConstantInfo(dis);
        }
        /* int accessFlags = */dis.readUnsignedShort();
        /* int thisClass = */dis.readUnsignedShort();
        /* int superClass = */dis.readUnsignedShort();
        interfaces = new int[dis.readUnsignedShort()];
        for (int i = 0; i < interfaces.length; i++) {
            interfaces[i] = dis.readUnsignedShort();
        }
        fields = new FieldInfo[dis.readUnsignedShort()];
        for (int i = 0; i < fields.length; i++) {
            fields[i] = parseFieldInfo(dis);
        }
        methods = new MethodInfo[dis.readUnsignedShort()];
        for (int i = 0; i < methods.length; i++) {
            methods[i] = parseMethodInfo(dis);
        }
        attributes = new Attribute[dis.readUnsignedShort()];
        for (int i = 0; i < attributes.length; i++) {
            attributes[i] = parseAttributeInfo(dis);
        }
    }

    private ConstantInfo parseConstantInfo(DataInputStream dis) throws IOException {
        ConstantInfo cinfo = new ConstantInfo();
        cinfo.tag = dis.readUnsignedByte();
        switch (cinfo.tag) {
        case ConstantType.Class:
            cinfo.nameIndex = dis.readUnsignedShort();
            break;
        case ConstantType.FieldRef:
        case ConstantType.MethodRef:
        case ConstantType.InterfaceMethodRef:
            cinfo.classIndex = dis.readUnsignedShort();
            cinfo.nameAndTypeIndex = dis.readUnsignedShort();
            break;
        case ConstantType.String:
            cinfo.stringIndex = dis.readUnsignedShort();
            break;
        case ConstantType.Integer:
        case ConstantType.Float:
            cinfo.bytes = dis.readInt();
            break;
        case ConstantType.Long:
        case ConstantType.Double:
            cinfo.highBytes = dis.readInt();
            cinfo.lowBytes = dis.readInt();
            break;
        case ConstantType.NameAndType:
            cinfo.nameIndex = dis.readUnsignedShort();
            cinfo.descriptorIndex = dis.readUnsignedShort();
            break;
        case ConstantType.Utf8:
            byte[] data = new byte[dis.readUnsignedShort()];
            dis.read(data);
            cinfo.string = new String(data);
            break;
        }
        return cinfo;
    }

    private FieldInfo parseFieldInfo(DataInputStream dis) throws IOException {
        FieldInfo finfo = new FieldInfo();
        finfo.accessFlags = dis.readUnsignedShort();
        finfo.nameIndex = dis.readUnsignedShort();
        finfo.descriptorIndex = dis.readUnsignedShort();
        finfo.attributes = new Attribute[dis.readUnsignedShort()];
        for (int i = 0; i < finfo.attributes.length; i++) {
            finfo.attributes[i] = parseAttributeInfo(dis);
        }
        return finfo;
    }

    private MethodInfo parseMethodInfo(DataInputStream dis) throws IOException {
        MethodInfo minfo = new MethodInfo();
        minfo.accessFlags = dis.readUnsignedShort();
        minfo.nameIndex = dis.readUnsignedShort();
        minfo.descriptorIndex = dis.readUnsignedShort();
        minfo.attributes = new Attribute[dis.readUnsignedShort()];
        for (int i = 0; i < minfo.attributes.length; i++) {
            minfo.attributes[i] = parseAttributeInfo(dis);
        }
        return minfo;
    }

    private Attribute parseAttributeInfo(DataInputStream dis) throws IOException {
        Attribute ainfo = null;
        int nameIndex = dis.readUnsignedShort();
        int length = dis.readInt();
        String type = constants[nameIndex - 1].string;
        if ("Code".equals(type)) {
            CodeAttribute mai = new CodeAttribute();
            mai.maxStack = dis.readUnsignedShort();
            mai.maxLocals = dis.readUnsignedShort();
            mai.code = new byte[dis.readInt()];
            dis.read(mai.code);
            mai.exceptionTable = new ExceptionTable[dis.readUnsignedShort()];
            for (int j = 0; j < mai.exceptionTable.length; j++) {
                mai.exceptionTable[j] = new ExceptionTable();
                mai.exceptionTable[j].startPC = dis.readUnsignedShort();
                mai.exceptionTable[j].endPC = dis.readUnsignedShort();
                mai.exceptionTable[j].handlerPC = dis.readUnsignedShort();
                mai.exceptionTable[j].catchType = dis.readUnsignedShort();
            }
            mai.attributes = new Attribute[dis.readUnsignedShort()];
            for (int j = 0; j < mai.attributes.length; j++) {
                mai.attributes[j] = parseAttributeInfo(dis);
            }
            ainfo = mai;
        } else if ("LocalVariableTable".equals(type)) {
            LocalVariableTableAttribute lvta = new LocalVariableTableAttribute();
            lvta.localVariableTable = new LocalVariableTable[dis.readUnsignedShort()];
            for (int i = 0; i < lvta.localVariableTable.length; i++) {
                lvta.localVariableTable[i] = new LocalVariableTable();
                lvta.localVariableTable[i].startPC = dis.readUnsignedShort();
                lvta.localVariableTable[i].length = dis.readUnsignedShort();
                lvta.localVariableTable[i].nameIndex = dis.readUnsignedShort();
                lvta.localVariableTable[i].descriptorIndex = dis.readUnsignedShort();
                lvta.localVariableTable[i].index = dis.readUnsignedShort();
            }
            ainfo = lvta;
        } else {
            ainfo = new Attribute();
            byte[] data = new byte[length];
            dis.read(data);
        }
        ainfo.nameIndex = nameIndex;
        ainfo.length = length;
        return ainfo;
    }

    public static class ConstantInfo
    {
        public int descriptorIndex;
        public int lowBytes;
        public int highBytes;
        public int bytes;
        public int tag;
        public int nameIndex;
        public int classIndex;
        public int nameAndTypeIndex;
        public int stringIndex;
        public String string;
    }

    public static class FieldInfo
    {
        public int accessFlags;
        public int nameIndex;
        public int descriptorIndex;
        public Attribute[] attributes;
    }

    public static class MethodInfo
    {
        public int accessFlags;
        public int nameIndex;
        public int descriptorIndex;
        public Attribute[] attributes;
    }

    public static class Attribute
    {
        public int nameIndex;
        public int length;
    }

    public static class CodeAttribute extends Attribute
    {
        public int maxStack;
        public int maxLocals;
        public byte[] code;
        public ExceptionTable[] exceptionTable;
        public Attribute[] attributes;
    }

    public static class LocalVariableTableAttribute extends Attribute
    {
        public LocalVariableTable[] localVariableTable;
    }

    public static class LocalVariableTable
    {
        public int startPC;
        public int length;
        public int nameIndex;
        public int descriptorIndex;
        public int index;
    }

    public static class ExceptionTable
    {
        public int startPC;
        public int endPC;
        public int handlerPC;
        public int catchType;
    }

    public static class ConstantType
    {
        public static final int Class = 7;
        public static final int FieldRef = 9;
        public static final int MethodRef = 10;
        public static final int InterfaceMethodRef = 11;
        public static final int String = 8;
        public static final int Integer = 3;
        public static final int Float = 4;
        public static final int Long = 5;
        public static final int Double = 6;
        public static final int NameAndType = 12;
        public static final int Utf8 = 1;
    }
}
