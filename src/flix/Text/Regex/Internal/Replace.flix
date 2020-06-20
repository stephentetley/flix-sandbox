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

use Text/Regex/Internal.{PrimMatcher, PrimMatchEvaluator};
use Text/Regex/Internal/PrimMatcher.{find, appendReplacement, appendTail};

namespace Text/Regex/Internal/Replace {

    /// We cannot wrap Java's `replaceAll​(Function<MatchResult,​String> replacer)` because we can't
    /// marshal functions to Java, s owe remake it.
    pub def replaceAllWith(x: PrimMatcher, eval: PrimMatchEvaluator[String]): Result[String, System.Error] & Impure =
        use Result.flatMap;
        let sb = StringBuilder.new();
        let* _ = replaceAllWithHelper(x, eval, sb);
        Ok(StringBuilder.toString(sb))

    pub def replaceAllWithHelper(x: PrimMatcher, eval: PrimMatchEvaluator[String], sb: StringBuilder): Result[Unit, System.Error] & Impure =
        use Result.flatMap;
        if (x.find() == false) {
            let _ = x.appendTail(sb);
            Ok()
        } else {
            let* rep = eval(x);
            let* _ = x.appendReplacement(sb, rep);
            replaceAllWithHelper(x, eval, sb)
        }

    pub def replaceAllWithIndex(x: PrimMatcher, eval: Int32 -> PrimMatchEvaluator[String]): Result[String, System.Error] & Impure =
        use Result.flatMap;
        let sb = StringBuilder.new();
        let* _ = replaceAllWithIndexHelper(x, eval, sb, 0);
        Ok(StringBuilder.toString(sb))

    pub def replaceAllWithIndexHelper(x: PrimMatcher, eval: Int32 -> PrimMatchEvaluator[String], sb: StringBuilder, ix: Int32): Result[Unit, System.Error] & Impure =
        use Result.flatMap;
        if (x.find() == false) {
            let _ = x.appendTail(sb);
            Ok()
        } else {
            let* rep = eval(ix,x);
            let* _ = x.appendReplacement(sb, rep);
            replaceAllWithIndexHelper(x, eval, sb, ix+1)
        }


}