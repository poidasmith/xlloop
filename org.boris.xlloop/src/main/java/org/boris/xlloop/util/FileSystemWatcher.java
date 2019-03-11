/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     Peter Smith
 *******************************************************************************/
package org.boris.xlloop.util;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class FileSystemWatcher
{
    private File baseDir;
    private boolean isStopped;
    private int pauseMillis = 60000; // one minute
    private Callback callback;
    private Thread thread;
    private Map files = new HashMap();
    private Map fileModificationTimes = new HashMap();

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
        Set fileSet = (Set) files.get(dir);
        if (fileSet == null) {
            fileSet = new HashSet();
            files.put(dir, fileSet);
        }
        Set fileCopy = new HashSet(fileSet);
        File[] flist = dir.listFiles();
        if (flist == null)
            return;
        for (int i = 0; i < flist.length; i++) {
            File f = flist[i];
            if (f.isDirectory()) {
                searchDir(f);
            } else {
                if (!fileSet.contains(f)) {
                    callback.fileAdded(f);
                    fileModificationTimes.put(f, new Long(f.lastModified()));
                    fileSet.add(f);
                    continue;
                } else {
                    fileCopy.remove(f);
                    Long mod = (Long) fileModificationTimes.get(f);
                    if (mod != null && mod.longValue() != f.lastModified()) {
                        fileModificationTimes
                                .put(f, new Long(f.lastModified()));
                        callback.fileChanged(f);
                        continue;
                    }
                }
            }
        }

        // Now look for removed files
        for (Iterator i = fileCopy.iterator(); i.hasNext();) {
            File f = (File) i.next();
            callback.fileRemoved(f);
            fileSet.remove(f);
        }
    }

    public interface Callback
    {
        void fileAdded(File f);

        void fileChanged(File f);

        void fileRemoved(File f);
    }

    public static class CallbackAdaptor implements Callback
    {
        public void fileAdded(File f) {
        }

        public void fileChanged(File f) {
        }

        public void fileRemoved(File f) {
        }
    }
}
