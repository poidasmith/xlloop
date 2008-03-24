package org.boris.functionserver.util;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class FileSystemWatcher {
    private File baseDir;
    private boolean isStopped;
    private int pauseMillis = 60000; // one minute
    private Callback callback;
    private Thread thread;
    private Map<File, Set<File>> files = new HashMap();
    private Map<File, Long> fileModificationTimes = new HashMap();

    public FileSystemWatcher(File baseDir, Callback callback) {
        this.thread = new Thread(new Runnable() {
            public void run() {
                FileSystemWatcher.this.run();
            }
        });
        thread.setDaemon(true);
        thread.setName("FileSystemWatcher:" + baseDir.getName());
        this.baseDir = baseDir;
        this.callback = callback;
    }

    public void setPauseMillis(int millis) {
        pauseMillis = millis;
    }

    public void start() {
        thread.start();
    }

    public void shutdown() {
        isStopped = true;
    }

    private void run() {
        while (!isStopped) {
            searchDir(baseDir);
            try {
                Thread.sleep(pauseMillis);
            } catch (InterruptedException e) {
            }
        }
    }

    private void searchDir(File dir) {
        Set<File> fileSet = files.get(dir);
        if (fileSet == null) {
            fileSet = new HashSet();
            files.put(dir, fileSet);
        }
        Set<File> fileCopy = new HashSet(fileSet);
        File[] flist = dir.listFiles();
        for (File f : flist) {
            if (f.isDirectory()) {
                searchDir(f);
            } else {
                if (!fileSet.contains(f)) {
                    callback.fileAdded(f);
                    fileModificationTimes.put(f, f.lastModified());
                    fileSet.add(f);
                    continue;
                } else {
                    fileCopy.remove(f);
                    Long mod = fileModificationTimes.get(f);
                    if (mod != null && mod.longValue() != f.lastModified()) {
                        fileModificationTimes.put(f, f.lastModified());
                        callback.fileChanged(f);
                        continue;
                    }
                }
            }
        }

        // Now look for removed files
        for (File f : fileCopy) {
            callback.fileRemoved(f);
            fileSet.remove(f);
        }
    }

    public interface Callback {
        void fileAdded(File f);

        void fileChanged(File f);

        void fileRemoved(File f);
    }
}
