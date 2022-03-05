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

package flixspt.sandbox;


import flixspt.sandbox.marshal.TupleStringString;

import java.util.Iterator;
import java.util.Map;

public class GetEnvIterator {
    private Iterator<Map.Entry<String, String>> iter;

    public GetEnvIterator() throws Exception {
        Map<String, String> env = System.getenv();
        this.iter = env.entrySet().iterator();
    }

    public boolean hasNext() {
        return this.iter.hasNext();
    }

    public TupleStringString next() {
        Map.Entry<String,String> item = this.iter.next();
        return new TupleStringString(item.getKey(), item.getValue());
    }

}
