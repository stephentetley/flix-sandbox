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



namespace Text/Collator {


    ///
    /// Test equality of strings `s1` and `s2` ignoring case.
    ///
    pub def equals(s1: String, s2: String): Bool =
        equalsHelper(s1, s2) as & Pure

    ///
    /// Helper function for `equalsCI`.
    ///
    /// Uses a Collator to be accurate with the locale of the JVM.
    ///
    def equalsHelper(s1: String, s2: String): Bool & Impure =
         import java.text.Collator:getInstance();
         import java.text.Collator.setStrength(Int32) as setStrength!;
         import java.text.Collator.compare(String, String);
         let coll = getInstance();
         coll.setStrength!(0);
         if (coll.compare(s1, s2) == 0) true else false

}