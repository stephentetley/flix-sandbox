package flix.runtime.library.system.marshal;

import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.stream.Stream;

public class UnmarshalFilesWalk {
    private Iterator<Path> iter;

    public UnmarshalFilesWalk(Path dir, boolean followLinks) throws Exception {
        if (followLinks) {
            Stream<Path> stream = Files.walk(dir, FileVisitOption.FOLLOW_LINKS);
            this.iter = stream.iterator();
        } else {
            Stream<Path> stream = Files.walk(dir);
            this.iter = stream.iterator();
        }
    }

    public boolean hasNext() {
        return this.iter.hasNext();
    }

    public Path next() { return this.iter.next(); }

}
