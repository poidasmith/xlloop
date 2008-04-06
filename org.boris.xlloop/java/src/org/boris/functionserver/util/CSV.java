package org.boris.functionserver.util;

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
}
