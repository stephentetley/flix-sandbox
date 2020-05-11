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

package flix.runtime.spt.sandbox.system.marshal;

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
