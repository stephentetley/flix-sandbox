/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package flix.runtime.spt.sandbox.system;

import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.stream.Stream;

public class FilesPathCursor {
    
    private Iterator<Path> iter;

    protected FilesPathCursor(Iterator<Path> iter1) {
        iter = iter1;
    }

    /// Use a protected constructor and factory methods so we can initialize different
    /// "processses" with the same cursor / iterator.

    /// Factory method for `Files.walk`
    public static FilesPathCursor createFilesWalkCursor(Path dir, boolean followLinks) throws Exception {
        Iterator<Path> iter1;
        if (followLinks) {
            Stream<Path> stream = Files.walk(dir, FileVisitOption.FOLLOW_LINKS);
            iter1 = stream.iterator();
        } else {
            Stream<Path> stream = Files.walk(dir);
            iter1 = stream.iterator();
        }
        return new FilesPathCursor(iter1);
    }

    /// Factory method for `Files.list`
    public static FilesPathCursor createFilesListCursor(Path dir) throws Exception {
        Stream<Path> stream = Files.list(dir);
        return new FilesPathCursor(stream.iterator());
    }


    public boolean hasNext() {
        return this.iter.hasNext();
    }

    public Path next() { return this.iter.next(); }

}
