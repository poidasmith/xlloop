package org.boris.xlloop;

import java.io.File;

import org.boris.xlloop.util.FileSystemWatcher;

public class FileSystemWatcherTest {
    public static void main(String[] args) throws Exception {
        FileSystemWatcher fs = new FileSystemWatcher(new File(
                "C:\\eclipse\\workspace\\org.boris.functionserver\\functions"),
                new FileSystemWatcher.Callback() {

                    public void fileAdded(File f) {
                        System.out.println("Added: " + f);
                    }

                    public void fileChanged(File f) {
                        System.out.println("Changed: " + f);
                    }

                    public void fileRemoved(File f) {
                        System.out.println("Removed: " + f);
                    }
                });

        fs.setPauseMillis(1000);
        fs.start();
    }
}
