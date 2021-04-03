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

import flix.runtime.spt.sandbox.system.marshal.MarshalStringArray;

import java.io.InputStream;
import java.nio.charset.Charset;

public class ProcessWrapper {

    public static ProcessBuilder create(String cmd, MarshalStringArray args) {
        args.cons(cmd);
        return new ProcessBuilder(args.toArray());
    }

    public static String getInput(Process proc, Charset cs) throws Exception {
        InputStream ins = proc.getInputStream();
        return new String(ins.readAllBytes(), cs);
    }

    public static String getError(Process proc, Charset cs) throws Exception {
        InputStream ins = proc.getErrorStream();
        return new String(ins.readAllBytes(), cs);
    }

}
