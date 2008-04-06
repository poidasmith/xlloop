package org.boris.functionserver;

import java.lang.reflect.Method;

import org.boris.functionserver.reflect.ParameterNameExtractor;
import org.boris.functionserver.util.CSV;

public class ParameterNameExtractorTest
{
    public static void main(String[] args) throws Exception {
        Class clazz = Math.class;
        ParameterNameExtractor pne = new ParameterNameExtractor(clazz);
        Method[] m = clazz.getMethods();
        for(int i = 0; i < m.length; i++) {
            if(m[i].getDeclaringClass().equals(clazz)) 
                System.out.println(m[i].getName() + "(" + CSV.toCSV(pne.getParameterNames(m[i])) + ")");
        }
    }
}
