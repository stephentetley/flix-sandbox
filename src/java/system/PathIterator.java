package flix.runtime.spt.sandbox.system;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;

public class PathIterator {

    private Iterator<Path> iter;

    protected PathIterator(Iterator<Path> iter1) {
        iter = iter1;
    }

    /// Factory method for `Files.list`
    public static PathIterator newFileslistIterator(Path dirpath) throws Exception {
        Iterator<Path> iter1 = Files.list(dirpath).iterator();
        return new PathIterator(iter1);
    }

    /// Factory method for `Files.newDirectoryStream`
    public static PathIterator newFilesnewDirectoryStreamIterator(Path dirpath, String glob) throws Exception {
        Iterator<Path> iter1 = Files.newDirectoryStream(dirpath, glob).iterator();
        return new PathIterator(iter1);
    }

    public boolean hasNext() {
        return this.iter.hasNext();
    }

    public Path next() { return this.iter.next(); }


}
