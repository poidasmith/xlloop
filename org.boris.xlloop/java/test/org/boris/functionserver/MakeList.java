package org.boris.functionserver;

public class MakeList
{
    public static void main(String[] args) throws Exception {
        for (int i = 0; i < 256; i++) {
            //System.out.println("\tFS" + i);
            System.out.println("DECLARE_EXCEL_FUNCTION(" + i  + ")");
        }
    }
}
