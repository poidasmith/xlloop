package org.boris.xlloop;

public class ConsoleTest {
    public static void main(String[] args) throws Exception {
        for(int i = 0; i < 100; i++) {
            System.out.print("\r");
            System.out.printf("testing: %d", i);
            Threads.sleep(100);
        }
        System.out.println();
    }
}
