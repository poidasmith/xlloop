package org.boris.xlloop;

public class MakeList
{
    public static void main(String[] args) throws Exception {
        for (int i = 0; i < 512; i++) {
            System.out.println("\tFS" + i + " @" + (10 + i));
            // System.out.println("DECLARE_EXCEL_FUNCTION(" + i + ")");
        }
    }
}
