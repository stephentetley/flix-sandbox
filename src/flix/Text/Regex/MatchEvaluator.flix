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



namespace Text/Regex {

    use Text/Regex/Internal.PrimMatcher;

    ///
    /// Matcher is a monad (reader, error).
    ///
    pub opaque type MatchEvaluator[a] = PrimMatcher ~> Result[a, String]

    /// TODO - can we make this effect polymorphic?

}

namespace Text/Regex/MatchEvaluator {

    use FromString.fromString;

    use Text/Regex.MatchEvaluator;
    use Text/Regex.MatchEvaluator.{MatchEvaluator};
    use Text/Regex/Internal.PrimMatcher;
    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};

    pub def return(x: a): MatchEvaluator[a] = 
        MatchEvaluator(_ -> Ok(x) as & Impure)

    def apply1(x: MatchEvaluator[a], m: PrimMatcher): Result[a, String] & Impure =
        let MatchEvaluator(f) = x;
        f(m)

    pub def throwError(msg: String): MatchEvaluator[b] = 
        MatchEvaluator(_ -> Err(msg) as & Impure)

    pub def swapError(mx: MatchEvaluator[a], msg: String): MatchEvaluator[a] =
        MatchEvaluator(r -> match apply1(mx, r) {
            case Ok(a) => Ok(a)
            case Err(_) => Err(msg)
        })

    pub def map(f: a -> b & ef, x: MatchEvaluator[a]): MatchEvaluator[b] =
        MatchEvaluator(r -> 
            match apply1(x, r) {
                case Ok(a) => {
                    let ans = f(a) as & Impure;
                    Ok(ans)
                }
                case Err(e) => Err(e)
            }
        )        

    pub def ap(mf: MatchEvaluator[a -> b & ef], ma: MatchEvaluator[a]): MatchEvaluator[b] =
        MatchEvaluator(r -> 
            match apply1(mf, r) {
                case Ok(f) => match apply1(ma, r) {
                    case Ok(a) => Ok(f(a) as & Impure)
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



    def liftOptionPrim(fn : PrimMatcher ~> Option[a], errMsg: String): MatchEvaluator[a] =
        MatchEvaluator(r -> Option.toOk(fn(r), errMsg))

    pub def liftOption(x: Option[a], errMsg: String): MatchEvaluator[a] = match x { 
        case Some(x1) => return(x1)
        case None => throwError(errMsg)
    }
        

    pub def liftResult(x: Result[a, String]): MatchEvaluator[a] =
        MatchEvaluator(_ -> x as & Impure)

    pub def liftOperation(fn: Unit ~> Result[a, String]): MatchEvaluator[a] =
        MatchEvaluator(_ -> fn())


    pub def content(): MatchEvaluator[String] = liftOptionPrim(Text/Regex/Internal/PrimMatcher.group, "content")

    /// TODO - is this worthwhile?
    pub def transformContent(f: String -> a): MatchEvaluator[a] = map(f, content())


    pub def getStart(): MatchEvaluator[Int32] = liftOptionPrim(Text/Regex/Internal/PrimMatcher.start, "getStart")

    pub def getEnd(): MatchEvaluator[Int32] = liftOptionPrim(Text/Regex/Internal/PrimMatcher.end, "getEnd")

    pub def getLength(): MatchEvaluator[Int32] =
        lift2((x,y) -> y - x, getStart(), getEnd()) `swapError` "getLength"

    ///
    ///
    pub def getRange(): MatchEvaluator[(Int32, Int32)] =
        lift2((x,y) -> (x,y), getStart(), getEnd()) `swapError` "getRange"


    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureAt(ix: Int32): MatchEvaluator[String] =
        liftOptionPrim(x -> Text/Regex/Internal/PrimMatcher.groupElementAt(x, ix+1), "getCaptureAt")

    pub def getCaptureNamed(label: String): MatchEvaluator[String] =
        liftOptionPrim(x -> Text/Regex/Internal/PrimMatcher.groupElementNamed(x, label), "getCaptureNamed")

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureAtStart(ix: Int32): MatchEvaluator[Int32] =
        liftOptionPrim(x -> Text/Regex/Internal/PrimMatcher.groupStartAt(x, ix+1), "getCaptureAtStart")

    pub def getCaptureNamedStart(label: String): MatchEvaluator[Int32] =
        liftOptionPrim(x -> Text/Regex/Internal/PrimMatcher.groupStartNamed(x, label), "getCaptureNamedStart")

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)`
    /// uses 1-indexing.
    ///
    pub def getCaptureAtEnd(ix: Int32): MatchEvaluator[Int32] =
        liftOptionPrim(x -> Text/Regex/Internal/PrimMatcher.groupEndAt(x, ix+1), "getCaptureAtEnd")

    pub def getCaptureNamedEnd(label: String): MatchEvaluator[Int32] =
        liftOptionPrim(x -> Text/Regex/Internal/PrimMatcher.groupEndNamed(x, label), "getCaptureNamedEnd")

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)`
    /// uses 1-indexing.
    ///
    pub def getCaptureAtRange(ix: Int32): MatchEvaluator[(Int32, Int32)] =
        lift2((x,y) -> (x,y), getCaptureAtStart(ix), getCaptureAtEnd(ix)) `swapError` "getCaptureAtRange"

    pub def getCaptureNamedRange(label: String): MatchEvaluator[(Int32, Int32)] =
        lift2((x,y) -> (x,y), getCaptureNamedStart(label), getCaptureNamedEnd(label))


    /// Note - type annos are a temporary fix while Flix's runtime cast error is resolved.

    pub def getInt8At(ix: Int32): MatchEvaluator[Int8] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[Int8], "getInt8At(${ix})")

    pub def getInt8Named(label: String): MatchEvaluator[Int8] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[Int8], "getInt8Named('${label}')")

    pub def getInt16At(ix: Int32): MatchEvaluator[Int16] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[Int16], "getInt16At(${ix})")

    pub def getInt16Named(label: String): MatchEvaluator[Int16] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[Int16], "getInt16Named('${label}')")

    pub def getInt32At(ix: Int32): MatchEvaluator[Int32] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[Int32], "getInt32At(${ix})")

    pub def getInt32Named(label: String): MatchEvaluator[Int32] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[Int32], "getInt32Named('${label}')")

    pub def getInt64At(ix: Int32): MatchEvaluator[Int64] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[Int64], "getInt64At(${ix})")

    pub def getInt64Named(label: String): MatchEvaluator[Int64] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[Int64], "getInt64Named('${label}')")

    pub def getBigIntAt(ix: Int32): MatchEvaluator[BigInt] =
        let* str = getCaptureAt(ix);
        liftOption(fromString(str): Option[BigInt], "getBigIntAt(${ix})")

    pub def getBigIntNamed(label: String): MatchEvaluator[BigInt] =
        let* str = getCaptureNamed(label);
        liftOption(fromString(str): Option[BigInt], "getBigIntNamed('${label}')")

    ///
    /// Applies the binary function `f` to the values in `o1` and `o2`.
    ///
    /// Returns an error if either `o1` or `o2` are errors.
    ///
    pub def lift2(f: (t1, t2) -> u & ef,
                  o1: MatchEvaluator[t1],
                  o2: MatchEvaluator[t2]): MatchEvaluator[u] =
        ap(map(f, o1), o2)

    ///
    /// Applies the ternary function `f` to the values in `o1`, `o2` and `o3`.
    ///
    /// Returns an error if any of `o1`, `o2` and `o3` are errors.
    ///
    pub def lift3(f: (t1, t2, t3) -> u & ef,
                  o1: MatchEvaluator[t1],
                  o2: MatchEvaluator[t2],
                  o3: MatchEvaluator[t3]): MatchEvaluator[u] =
        ap(lift2(f, o1, o2), o3)

    ///
    /// Applies the 4-ary function `f` to the values in `o1`, `o2`, `o3` and `o4`.
    ///
    /// Returns an error if any of `o1`, `o2`, `o3` and `o4` are errors.
    ///
    pub def lift4(f: (t1, t2, t3, t4) -> u & ef,
                  o1: MatchEvaluator[t1],
                  o2: MatchEvaluator[t2],
                  o3: MatchEvaluator[t3],
                  o4: MatchEvaluator[t4]): MatchEvaluator[u] =
        ap(lift3(f, o1, o2, o3), o4)

    ///
    /// Applies the 5-ary function `f` to the values in `o1`, `o2`, ... `o5`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o5` are errors.
    ///
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u & ef,
                  o1: MatchEvaluator[t1],
                  o2: MatchEvaluator[t2],
                  o3: MatchEvaluator[t3],
                  o4: MatchEvaluator[t4],
                  o5: MatchEvaluator[t5]): MatchEvaluator[u] =
        ap(lift4(f, o1, o2, o3, o4), o5)

    ///
    /// Applies the 6-ary function `f` to the values in `o1`, `o2`, ... `o6`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o6` are errors.
    ///
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u & ef,
                  o1: MatchEvaluator[t1],
                  o2: MatchEvaluator[t2],
                  o3: MatchEvaluator[t3],
                  o4: MatchEvaluator[t4],
                  o5: MatchEvaluator[t5],
                  o6: MatchEvaluator[t6]): MatchEvaluator[u] =
        ap(lift5(f, o1, o2, o3, o4, o5), o6)

    ///
    /// Applies the 7-ary function `f` to the values in `o1`, `o2`, ... `o7`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o7` are errors.
    ///
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u & ef,
                  o1: MatchEvaluator[t1],
                  o2: MatchEvaluator[t2],
                  o3: MatchEvaluator[t3],
                  o4: MatchEvaluator[t4],
                  o5: MatchEvaluator[t5],
                  o6: MatchEvaluator[t6],
                  o7: MatchEvaluator[t7]): MatchEvaluator[u] =
        ap(lift6(f, o1, o2, o3, o4, o5, o6), o7)

    ///
    /// Applies the 8-ary function `f` to the values in `o1`, `o2`, ... `o8`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o8` are errors.
    ///
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u & ef,
                  o1: MatchEvaluator[t1],
                  o2: MatchEvaluator[t2],
                  o3: MatchEvaluator[t3],
                  o4: MatchEvaluator[t4],
                  o5: MatchEvaluator[t5],
                  o6: MatchEvaluator[t6],
                  o7: MatchEvaluator[t7],
                  o8: MatchEvaluator[t8]): MatchEvaluator[u] =
        ap(lift7(f, o1, o2, o3, o4, o5, o6, o7), o8)

    ///
    /// Applies the 9-ary function `f` to the values in `o1`, `o2`, ... `o9`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o9` are errors.
    ///
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u & ef,
                  o1: MatchEvaluator[t1],
                  o2: MatchEvaluator[t2],
                  o3: MatchEvaluator[t3],
                  o4: MatchEvaluator[t4],
                  o5: MatchEvaluator[t5],
                  o6: MatchEvaluator[t6],
                  o7: MatchEvaluator[t7],
                  o8: MatchEvaluator[t8],
                  o9: MatchEvaluator[t9]): MatchEvaluator[u] =
        ap(lift8(f, o1, o2, o3, o4, o5, o6, o7, o8), o9)

    ///
    /// Applies the 10-ary function `f` to the values in `o1`, `o2`, ... `o10`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o10` are errors.
    ///
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u & ef,
                   o1: MatchEvaluator[t1],
                   o2: MatchEvaluator[t2],
                   o3: MatchEvaluator[t3],
                   o4: MatchEvaluator[t4],
                   o5: MatchEvaluator[t5],
                   o6: MatchEvaluator[t6],
                   o7: MatchEvaluator[t7],
                   o8: MatchEvaluator[t8],
                   o9: MatchEvaluator[t9],
                   o10: MatchEvaluator[t10]): MatchEvaluator[u] =
        ap(lift9(f, o1, o2, o3, o4, o5, o6, o7, o8, o9), o10)


    ///
    /// Returns a 2 element tuple with the results of applying the evaluators `o1` and `o2`.
    ///
    /// Returns an error if evaluating either `o1` or `o2` is an error.
    ///
    pub def tuple2(o1: MatchEvaluator[t1],
                   o2: MatchEvaluator[t2]): MatchEvaluator[(t1, t2)] =
        lift2((x,y) -> (x,y), o1, o2)

    ///
    /// Returns a 3 element tuple with the results of applying the evaluators `o1`, `o2` and `o3`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2` and `o3` is an error.
    ///
    pub def tuple3(o1: MatchEvaluator[t1],
                   o2: MatchEvaluator[t2],
                   o3: MatchEvaluator[t3]): MatchEvaluator[(t1, t2, t3)] =
        lift3((x1, x2, x3) -> (x1, x2, x3), o1, o2, o3)

    ///
    /// Returns a 4 element tuple with the results of applying the evaluators `o1`, `o2`, `o3` and `o4`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, `o3` and `o4` is an error.
    ///
    pub def tuple4(o1: MatchEvaluator[t1],
                   o2: MatchEvaluator[t2],
                   o3: MatchEvaluator[t3],
                   o4: MatchEvaluator[t4]): MatchEvaluator[(t1, t2, t3, t4)]  =
        lift4((x1, x2, x3, x4) -> (x1, x2, x3, x4), o1, o2, o3, o4)

    ///
    /// Returns a 5 element tuple with the results of applying the evaluators `o1`, `o2`, ... `o5`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o5` is an error.
    ///
    pub def tuple5(o1: MatchEvaluator[t1],
                   o2: MatchEvaluator[t2],
                   o3: MatchEvaluator[t3],
                   o4: MatchEvaluator[t4],
                   o5: MatchEvaluator[t5]): MatchEvaluator[(t1, t2, t3, t4, t5)]  =
        lift5((x1, x2, x3, x4, x5) -> (x1, x2, x3, x4, x5), o1, o2, o3, o4, o5)

    ///
    /// Returns a 6 element tuple with the results of applying the evaluators `o1`, `o2`, ... `o6`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o6` is an error.
    ///
    pub def tuple6(o1: MatchEvaluator[t1],
                   o2: MatchEvaluator[t2],
                   o3: MatchEvaluator[t3],
                   o4: MatchEvaluator[t4],
                   o5: MatchEvaluator[t5],
                   o6: MatchEvaluator[t6]): MatchEvaluator[(t1, t2, t3, t4, t5, t6)]  =
        lift6((x1, x2, x3, x4, x5, x6) -> (x1, x2, x3, x4, x5, x6), o1, o2, o3, o4, o5, o6)

    ///
    /// Returns a 7 element tuple with the results of applying the evaluators `o1`, `o2`, ... `o7`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o7` is an error.
    ///
    pub def tuple7(o1: MatchEvaluator[t1],
                   o2: MatchEvaluator[t2],
                   o3: MatchEvaluator[t3],
                   o4: MatchEvaluator[t4],
                   o5: MatchEvaluator[t5],
                   o6: MatchEvaluator[t6],
                   o7: MatchEvaluator[t7]): MatchEvaluator[(t1, t2, t3, t4, t5, t6, t7)]  =
        lift7((x1, x2, x3, x4, x5, x6, x7) -> (x1, x2, x3, x4, x5, x6, x7), o1, o2, o3, o4, o5, o6, o7)

    ///
    /// Returns a 8 element tuple with the results of applying the evaluators `o1`, `o2`, ... `o8`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o8` is an error.
    ///
    pub def tuple8(o1: MatchEvaluator[t1],
                   o2: MatchEvaluator[t2],
                   o3: MatchEvaluator[t3],
                   o4: MatchEvaluator[t4],
                   o5: MatchEvaluator[t5],
                   o6: MatchEvaluator[t6],
                   o7: MatchEvaluator[t7],
                   o8: MatchEvaluator[t8]): MatchEvaluator[(t1, t2, t3, t4, t5, t6, t7, t8)]  =
        lift8((x1, x2, x3, x4, x5, x6, x7, x8) -> (x1, x2, x3, x4, x5, x6, x7, x8), o1, o2, o3, o4, o5, o6, o7, o8)

    ///
    /// Returns a 9 element tuple with the results of applying the evaluators `o1`, `o2`, ... `o9`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o9` is an error.
    ///
    pub def tuple9(o1: MatchEvaluator[t1],
                   o2: MatchEvaluator[t2],
                   o3: MatchEvaluator[t3],
                   o4: MatchEvaluator[t4],
                   o5: MatchEvaluator[t5],
                   o6: MatchEvaluator[t6],
                   o7: MatchEvaluator[t7],
                   o8: MatchEvaluator[t8],
                   o9: MatchEvaluator[t9]): MatchEvaluator[(t1, t2, t3, t4, t5, t6, t7, t8, t9)]  =
        lift9((x1, x2, x3, x4, x5, x6, x7, x8, x9) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9), o1, o2, o3, o4, o5, o6, o7, o8, o9)

    ///
    /// Returns a 10 element tuple with the results of applying the evaluators `o1`, `o2`, ... `o10`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o10` is an error.
    ///
    pub def tuple10(o1: MatchEvaluator[t1],
                    o2: MatchEvaluator[t2],
                    o3: MatchEvaluator[t3],
                    o4: MatchEvaluator[t4],
                    o5: MatchEvaluator[t5],
                    o6: MatchEvaluator[t6],
                    o7: MatchEvaluator[t7],
                    o8: MatchEvaluator[t8],
                    o9: MatchEvaluator[t9],
                    o10: MatchEvaluator[t10]): MatchEvaluator[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)]  =
        lift10((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), o1, o2, o3, o4, o5, o6, o7, o8, o9, o10)

}