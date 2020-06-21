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

use System.Error.{PatternSyntaxException, IllegalStateException};

use Text/Regex.MatchEvaluator;
use Text/Regex.MatchEvaluator.{MatchEvaluator};
use Text/Regex/Internal.PrimMatcher;
use Text/Regex.Pattern;
use Text/Regex.Pattern.{Pattern};

namespace Text/Regex {

    ///
    /// Matcher is a monad (reader, error).
    ///
    pub opaque type MatchEvaluator[a] = PrimMatcher ~> Result[a, System.Error]

}

namespace Text/Regex/MatchEvaluator {


    
    pub def return(x: a): MatchEvaluator[a] = 
        MatchEvaluator(_ -> Ok(x) as & Impure)

    def apply1(x: MatchEvaluator[a], m: PrimMatcher): Result[a, System.Error] & Impure =
        let MatchEvaluator(f) = x;
        f(m)

    pub def map(f: a -> b & e, x: MatchEvaluator[a]): MatchEvaluator[b] =
        MatchEvaluator(r -> 
            match apply1(x, r) {
                case Ok(a) => {
                    let ans = f(a) as & Impure;
                    Ok(ans)
                }
                case Err(e) => Err(e)
            }
        )        

    pub def ap(mf: MatchEvaluator[a -> b], ma: MatchEvaluator[a]): MatchEvaluator[b] =
        MatchEvaluator(r -> 
            match apply1(mf, r) {
                case Ok(f) => match apply1(ma, r) {
                    case Ok(a) => Ok(f(a))
                    case Err(e2) => Err(e2)
                }
                case Err(e1) => Err(e1)
            }
        )

    pub def bind(x: MatchEvaluator[a], k: a -> MatchEvaluator[b]): MatchEvaluator[b] = 
        MatchEvaluator(r -> 
            match apply1(x, r) {
                case Ok(a) => apply1(k(a), r)
                case Err(e) => Err(e)
            }
        )

    pub def flatMap(k: a -> MatchEvaluator[b], x: MatchEvaluator[a]): MatchEvaluator[b] = bind(x, k)



    def liftPrim(fn : PrimMatcher ~> Result[a, System.Error]): MatchEvaluator[a] =
        MatchEvaluator(r -> fn(r))



    pub def content(): MatchEvaluator[String] = liftPrim(Text/Regex/Internal/PrimMatcher.group)

    pub def transformContent(f: String -> String): MatchEvaluator[String] = map(f, content())


    pub def getStart(): MatchEvaluator[Int32] = liftPrim(Text/Regex/Internal/PrimMatcher.start)

    pub def getEnd(): MatchEvaluator[Int32] = liftPrim(Text/Regex/Internal/PrimMatcher.end)

    ///
    ///
    pub def getRange(): MatchEvaluator[(Int32, Int32)] =
        lift2((x,y) -> (x,y), getStart(), getEnd())


    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCapture(ix: Int32): MatchEvaluator[String] =
        liftPrim(x -> Text/Regex/Internal/PrimMatcher.groupElementByIndex(x, ix+1))

    pub def getNamedCapture(label: String): MatchEvaluator[String] =
        liftPrim(x -> Text/Regex/Internal/PrimMatcher.groupElementByName(x, label))

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureStart(ix: Int32): MatchEvaluator[Int32] =
        liftPrim(x -> Text/Regex/Internal/PrimMatcher.groupStartByIndex(x, ix+1))

    pub def getNamedCaptureStart(label: String): MatchEvaluator[Int32] =
        liftPrim(x -> Text/Regex/Internal/PrimMatcher.groupStartByName(x, label))

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureEnd(ix: Int32): MatchEvaluator[Int32] =
        liftPrim(x -> Text/Regex/Internal/PrimMatcher.groupEndByIndex(x, ix+1))

    pub def getNamedCaptureEnd(label: String): MatchEvaluator[Int32] =
        liftPrim(x -> Text/Regex/Internal/PrimMatcher.groupEndByName(x, label))

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureRange(ix: Int32): MatchEvaluator[(Int32, Int32)] =
        lift2((x,y) -> (x,y), getCaptureStart(ix), getCaptureEnd(ix))

    pub def getNamedCaptureRange(label: String): MatchEvaluator[(Int32, Int32)] =
        lift2((x,y) -> (x,y), getNamedCaptureStart(label), getNamedCaptureEnd(label))

    ///
    /// Applies the binary function `f` to the values in `o1` and `o2`.
    ///
    /// Returns an error if either `o1` or `o2` are errors.
    ///
    pub def lift2(f: (t1, t2) -> u & f, o1: MatchEvaluator[t1], o2: MatchEvaluator[t2]): MatchEvaluator[u] =
        ap(map(f, o1), o2)

    ///
    /// Applies the ternary function `f` to the values in `o1`, `o2` and `o3`.
    ///
    /// Returns an error if any of `o1`, `o2` and `o3` are errors.
    ///
    pub def lift3(f: (t1, t2, t3) -> u & f, o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3]): MatchEvaluator[u] & f =
        ap(lift2(f, o1, o2), o3)

    ///
    /// Applies the 4-ary function `f` to the values in `o1`, `o2`, `o3` and `o4`.
    ///
    /// Returns an error if any of `o1`, `o2`, `o3` and `o4` are errors.
    ///
    pub def lift4(f: (t1, t2, t3, t4) -> u & f, o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3], o4: MatchEvaluator[t4]): MatchEvaluator[u] & f =
        ap(lift3(f, o1, o2, o3), o4)

    ///
    /// Applies the 5-ary function `f` to the values in `o1`, `o2`, ... `o5`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o5` are errors.
    ///
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u & f, o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3], o4: MatchEvaluator[t4], o5: MatchEvaluator[t5]): MatchEvaluator[u] & f =
        ap(lift4(f, o1, o2, o3, o4), o5)

    ///
    /// Applies the 6-ary function `f` to the values in `o1`, `o2`, ... `o6`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o6` are errors.
    ///
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u & f, o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3], o4: MatchEvaluator[t4], o5: MatchEvaluator[t5], o6: MatchEvaluator[t6]): MatchEvaluator[u] & f =
        ap(lift5(f, o1, o2, o3, o4, o5), o6)

    ///
    /// Applies the 7-ary function `f` to the values in `o1`, `o2`, ... `o7`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o7` are errors.
    ///
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u & f, o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3], o4: MatchEvaluator[t4], o5: MatchEvaluator[t5], o6: MatchEvaluator[t6], o7: MatchEvaluator[t7]): MatchEvaluator[u] & f=
        ap(lift6(f, o1, o2, o3, o4, o5, o6), o7)

    ///
    /// Applies the 8-ary function `f` to the values in `o1`, `o2`, ... `o8`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o8` are errors.
    ///
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u & f, o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3], o4: MatchEvaluator[t4], o5: MatchEvaluator[t5], o6: MatchEvaluator[t6], o7: MatchEvaluator[t7], o8: MatchEvaluator[t8]): MatchEvaluator[u] & f =
        ap(lift7(f, o1, o2, o3, o4, o5, o6, o7), o8)

    ///
    /// Applies the 9-ary function `f` to the values in `o1`, `o2`, ... `o9`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o9` are errors.
    ///
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u & f, o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3], o4: MatchEvaluator[t4], o5: MatchEvaluator[t5], o6: MatchEvaluator[t6], o7: MatchEvaluator[t7], o8: MatchEvaluator[t8], o9: MatchEvaluator[t9]): MatchEvaluator[u] & f =
        ap(lift8(f, o1, o2, o3, o4, o5, o6, o7, o8), o9)

    ///
    /// Applies the 10-ary function `f` to the values in `o1`, `o2`, ... `o10`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o10` are errors.
    ///
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u & f, o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3], o4: MatchEvaluator[t4], o5: MatchEvaluator[t5], o6: MatchEvaluator[t6], o7: MatchEvaluator[t7], o8: MatchEvaluator[t8], o9: MatchEvaluator[t9], o10: MatchEvaluator[t10]): MatchEvaluator[u] & f =
        ap(lift9(f, o1, o2, o3, o4, o5, o6, o7, o8, o9), o10)


    ///
    /// Returns a 2 element tuple with the results of applying the evaluators `o1` and `o2`.
    ///
    /// Returns an error if evaluating either `o1` or `o2` is an error.
    ///
    pub def tuple2(o1: MatchEvaluator[t1], o2: MatchEvaluator[t2]): MatchEvaluator[(t1, t2)] =
        lift2((x,y) -> (x,y), o1, o2)

    ///
    /// Returns a 3 element tuple with the results of applying the evaluators `o1`, `o2` and `o3`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2` and `o3` is an error.
    ///
    pub def tuple3(o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3]): MatchEvaluator[(t1, t2, t3)] =
        lift3((x1, x2, x3) -> (x1, x2, x3), o1, o2, o3)

    ///
    /// Returns a 4 element tuple with the results of applying the evaluators `o1`, `o2`, `o3` and `o4`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, `o3` and `o4` is an error.
    ///
    pub def tuple4(o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3], o4: MatchEvaluator[t4]): MatchEvaluator[(t1, t2, t3, t4)]  =
        lift4((x1, x2, x3, x4) -> (x1, x2, x3, x4), o1, o2, o3, o4)

    ///
    /// Returns a 5 element tuple with the results of applying the evaluators `o1`, `o2`, ... `o5`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o5` is an error.
    ///
    pub def tuple5(o1: MatchEvaluator[t1], o2: MatchEvaluator[t2], o3: MatchEvaluator[t3], o4: MatchEvaluator[t4], o5: MatchEvaluator[t5]): MatchEvaluator[(t1, t2, t3, t4, t5)]  =
        lift5((x1, x2, x3, x4, x5) -> (x1, x2, x3, x4, x5), o1, o2, o3, o4, o5)


}