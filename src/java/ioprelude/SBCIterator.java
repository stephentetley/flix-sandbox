/*
 * Copyright 2021 Stephen Tetley
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

package flix.runtime.spt.sandbox.ioprelude;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

public class SBCIterator {
    private SeekableByteChannel sbc;
    private int chunkSize;

    public SBCIterator(Path filepath, int chunkLen) throws IOException {
        this.sbc = Files.newByteChannel(filepath, StandardOpenOption.READ);
        this.chunkSize = chunkLen;
    }

    public boolean hasNext() {
        try {
            return (sbc.position() < sbc.size());
        } catch (Exception e) {
            return false;
        }
    }
    public int nextBufferSize() {
        try {
            long remaining = sbc.size() - sbc.position();
            if (remaining > chunkSize)
                return chunkSize;
            else
                return (int) remaining;

        } catch (Exception e) {
            return -1;
        }
    }

    public byte[] next() {
        try {
            ByteBuffer bb1 = ByteBuffer.allocate(chunkSize);
            int nread = sbc.read(bb1);
            return bb1.array();
        } catch (Exception e) {
            return null;
        }
    }

    public void close() throws Exception {
        sbc.close();
    }

}
