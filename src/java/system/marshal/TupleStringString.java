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

public class TupleStringString {
    private String s1;
    private String s2;

    public TupleStringString(String str1, String str2) {
        this.s1 = str1;
        this.s2 = str2;
    }

    public String get0() {
        return s1;
    }

    public String get1() {
        return s2;
    }
}
