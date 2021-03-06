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

import java.util.LinkedList;

public class MarshalStringArray {

    private LinkedList<String> cells;

    public MarshalStringArray() {
        this.cells = new LinkedList<String>();
    }

    public void snoc(String s) {
        this.cells.addLast(s);
    }

    public void cons(String s) {
        this.cells.addFirst(s);
    }

    public String[] toArray() {
        return this.cells.toArray(new String[this.cells.size()]);
    }

}
