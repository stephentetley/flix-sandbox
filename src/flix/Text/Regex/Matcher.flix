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


/// This is a "steppable" matcher like .Net `Regex.Match` but wrapped as a monad.

namespace Text/Regex {

    use Text/Regex/Internal.{PrimMatcher};

    pub opaque type Matcher[a] = PrimMatcher ~> Result[a, String]

}

namespace Text/Regex/Matcher {


    use Text/Regex.{Matcher};
    use Text/Regex.Matcher.{Matcher};

    use Text/Regex/Internal.{PrimMatcher};
    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};
    use Text/Regex.{MatchEvaluator};
    use Text/Regex.MatchEvaluator.{MatchEvaluator};

    pub def runMatcher(ma: Matcher[a], patt: Pattern, input: String): Result[a, String] & Impure =
        use Result.flatMap;
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher};
        let Matcher(m1) = ma;
        let Pattern(p1) = patt;
        let prim = newPrimMatcher(p1, input);
        m1(prim)

    ///
    /// Version of `runMatcher` that takes a regex string and compiles it to a Pattern.
    ///
    pub def runsMatcher(ma: Matcher[a], regex: String, input: String): Result[a, String] & Impure =
        use Result.flatMap;
        let* patt = Option.toOk(Text/Regex/Pattern.compile(regex), "runsMatcher - invalid regex: ${regex}");
        runMatcher(ma, patt, input)


    pub def return(x: a): Matcher[a] =
        Matcher(_ -> Ok(x) as & Impure)

    pub def fail(msg: String): Matcher[a] =
        Matcher(_ -> Err(msg) as & Impure)

    def apply1(x: Matcher[a], m: PrimMatcher): Result[a, String] & Impure =
        let Matcher(f) = x;
        f(m)

    pub def map(f: a -> b & e, x: Matcher[a]): Matcher[b] =
        Matcher(r ->
            match apply1(x, r) {
                case Ok(a) => {
                    let ans = f(a) as & Impure;
                    Ok(ans)
                }
                case Err(e) => Err(e)
            }
        )

    pub def ap(mf: Matcher[a -> b], ma: Matcher[a]): Matcher[b] =
        Matcher(r ->
            match apply1(mf, r) {
                case Ok(f) => match apply1(ma, r) {
                    case Ok(a) => Ok(f(a))
                    case Err(e2) => Err(e2)
                }
                case Err(e1) => Err(e1)
            }
        )

    pub def bind(x: Matcher[a], k: a -> Matcher[b]): Matcher[b] =
        Matcher(r ->
            match apply1(x, r) {
                case Ok(a) => apply1(k(a), r)
                case Err(e) => Err(e)
            }
        )

    pub def flatMap(k: a -> Matcher[b], x: Matcher[a]): Matcher[b] = bind(x, k)

    def liftPrim(fn : PrimMatcher ~> Result[a, String]): Matcher[a] =
        Matcher(r -> fn(r))

    pub def ignore(x: Matcher[a]): Matcher[Unit] = map(_ -> (), x)

    pub def matches(): Matcher[Bool] =
        liftPrim(x -> Ok(Text/Regex/Internal/PrimMatcher.matches(x)))

    pub def lookingAt(): Matcher[Bool] =
        liftPrim(x -> Ok(Text/Regex/Internal/PrimMatcher.lookingAt(x)))

    pub def find(): Matcher[Bool] =
        liftPrim(x -> Ok(Text/Regex/Internal/PrimMatcher.find(x)))

    pub def findFrom(start: Int32): Matcher[Bool] =
        liftPrim(x -> Ok(Text/Regex/Internal/PrimMatcher.findFrom(x, start)))

    pub def content(): Matcher[String] =
        liftPrim(Text/Regex/Internal/PrimMatcher.group)

    pub def evaluate(eval: MatchEvaluator[a]): Matcher[a] =
        let MatchEvaluator(fn) = eval;
        Matcher(r -> fn(r))

    pub def getMatch(): Matcher[String] =
        let* ans = find();
        if (ans)
            content()
        else
            fail("getMatch")

    def ifThenElse(cond: Matcher[s], thenm: s -> Matcher[t], elsem: Matcher[t]): Matcher[t] =
        Matcher(r ->
            match apply1(cond, r) {
                case Ok(a) => apply1(thenm(a), r)
                case Err(_) => apply1(elsem, r)
            }
        )

    pub def done(): Matcher[Bool] = ifThenElse(content(), _ -> return(false), return(true))


    pub def reset!(): Matcher[Unit] =
        liftPrim(x -> Ok(Text/Regex/Internal/PrimMatcher.reset(x)))

    pub def resetInput!(input: String): Matcher[Unit] =
        liftPrim(x -> Ok(Text/Regex/Internal/PrimMatcher.resetInput(x, input)))

    pub def setRegion!(start: Int32, end: Int32): Matcher[Unit] =
        liftPrim(x -> Text/Regex/Internal/PrimMatcher.region(x, start, end))

    pub def setPattern!(patt: Pattern): Matcher[Unit] =
        let Pattern(p1) = patt;
        liftPrim(x -> Ok(Text/Regex/Internal/PrimMatcher.usePattern(x, p1)))

    pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, ma: Matcher[a]): Matcher[ac] =
        if (n <= 0)
            return(acc)
        else {
            let* a = ma;
            countWith(append, append(a, acc), n-1, ma)
        }

    pub def count(n: Int32, ma: Matcher[a]): Matcher[List[a]] =
        use Data/Transient/DList.{empty, snoc, toList};
        map(toList, countWith((x, xs) -> snoc(xs, x), empty(), n, ma))


}        



