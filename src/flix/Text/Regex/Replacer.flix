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

use System.Error.{IllegalStateException, NoSuchElementException, Generic};

use Text/Regex.{Replacer};
use Text/Regex.Replacer.{Replacer};

use Text/Regex/Internal.{PrimMatcher};
use Text/Regex.Pattern;
use Text/Regex.Pattern.{Pattern};
use Text/Regex.{MatchEvaluator};
use Text/Regex.MatchEvaluator.{MatchEvaluator};

/// This is a "steppable" replacer like Matcher.
/// Index is nuber of `finds` not the number of replacements.

namespace Text/Regex {
    
    pub opaque type Replacer[a] = (StringBuilder, PrimMatcher, Int32) ~> Result[(a, Int32), System.Error]

}

namespace Text/Regex/Replacer {

    pub def runReplacer(rep: Replacer[a], patt: Pattern, input: String): Result[(a, String), System.Error] & Impure =
        use Result.flatMap;
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher};
        let sb = StringBuilder.new();
        let Replacer(r1) = rep;
        let Pattern(p1) = patt;
        let prim = newPrimMatcher(p1, input);
        /// Start index at -1 because we haven't fund anything yet...
        match r1(sb, prim, -1) {
            case Ok(a, _) => {
                let s = StringBuilder.toString(sb);
                Ok((a, s))
            }
            case Err(e) => Err(e)
        }

    pub def execReplacer(rep: Replacer[a], patt: Pattern, input: String): Result[String, System.Error] & Impure =
        runReplacer(rep, patt, input) |> Result.map(snd)

    ///
    /// Version of `runReplacer` that takes a regex string and compiles it to a Pattern.
    ///
    pub def runsReplacer(rep: Replacer[a], regex: String, input: String): Result[(a, String), System.Error] & Impure =
        use Result.flatMap;
        let* patt = Text/Regex/Pattern.compile(regex);
        runReplacer(rep, patt, input)

    pub def execsReplacer(rep: Replacer[a], regex: String, input: String): Result[String, System.Error] & Impure =
        runsReplacer(rep, regex, input) |> Result.map(snd)

    pub def return(x: a): Replacer[a] =
        Replacer((_, _, n) -> Ok(x, n) as & Impure)

    pub def fail(msg: String): Replacer[a] =
        Replacer((_, _, _) -> Err(Generic(msg)) as & Impure)

    def apply1(rep: Replacer[a], sb: StringBuilder, m: PrimMatcher, n: Int32): Result[(a, Int32), System.Error] & Impure =
        let Replacer(f) = rep;
        f(sb, m, n)

    pub def map(f: a -> b & e, rep: Replacer[a]): Replacer[b] =
        Replacer((sb, m, n) ->
            match apply1(rep, sb, m, n) {
                case Ok((a, n1)) => {
                    let a1 = f(a) as & Impure;
                    Ok(a1, n1)
                }
                case Err(e) => Err(e)
            }
        )

    pub def ap(repf: Replacer[a -> b], rep: Replacer[a]): Replacer[b] =
        Replacer((sb, m, n) ->
            match apply1(repf, sb, m, n) {
                case Ok((f, n1)) => match apply1(rep, sb, m, n1) {
                    case Ok((a, n2)) => Ok((f(a), n2))
                    case Err(e2) => Err(e2)
                }
                case Err(e1) => Err(e1)
            }
        )

    pub def bind(rep: Replacer[a], k: a -> Replacer[b]): Replacer[b] =
        Replacer((sb, m, n) ->
            match apply1(rep, sb, m, n) {
                case Ok((a, n1)) => apply1(k(a), sb , m, n1)
                case Err(e) => Err(e)
            }
        )

    pub def flatMap(k: a -> Replacer[b], x: Replacer[a]): Replacer[b] = bind(x, k)

    pub def ignore(x: Replacer[a]): Replacer[Unit] = map(_ -> (), x)


    def liftPrim(fn : PrimMatcher ~> Result[a, System.Error]): Replacer[a] =
        Replacer((_, m, n) -> Result.map(x -> (x, n), fn(m)))

    def liftWriterPrim(fn : (PrimMatcher, StringBuilder) ~> Result[a, System.Error]): Replacer[a] =
        Replacer((sb, m, n) -> Result.map(x -> (x, n), fn(m, sb)))

    pub def appendTail!(): Replacer[Unit] =
        use Text/Regex/Internal/PrimMatcher.{appendTail};
        liftWriterPrim((m,sb) -> Ok(appendTail(m, sb)))

    pub def appendReplacement!(replacement: String): Replacer[Unit] =
        use Text/Regex/Internal/PrimMatcher.{appendReplacement};
        liftWriterPrim((m,sb) -> appendReplacement(m, sb, replacement))

    pub def getIndex(): Replacer[Int32] =
        Replacer((_, _, n) -> Ok((n, n)) as & Impure)

    def incrementFinds!(incr: Bool): Replacer[Unit] =
        Replacer((_, _, n) -> Ok(((), if (incr) n + 1 else n)) as & Impure)

    pub def find(): Replacer[Bool] =
        let* bfound = liftPrim(x -> Ok(Text/Regex/Internal/PrimMatcher.find(x)));
        let* _ = incrementFinds!(bfound);
        return(bfound)

    pub def findFrom(start: Int32): Replacer[Bool] =
        let* bfound = liftPrim(x -> Ok(Text/Regex/Internal/PrimMatcher.findFrom(x, start)));
        let* _ = incrementFinds!(bfound);
        return(bfound)

    pub def content(): Replacer[String] =
        liftPrim(Text/Regex/Internal/PrimMatcher.group)

    pub def evaluate(eval: MatchEvaluator[a]): Replacer[a] =
        let MatchEvaluator(fn) = eval;
        liftPrim(fn)

    pub def replaceStep!(fn: String -> String): Replacer[Unit] =
        let* ans = find();
        if (ans) {
            let* s = content();
            appendReplacement!(fn(s))
        } else
            fail("replaceStep!")

    pub def evaluateReplace!(eval: MatchEvaluator[String]): Replacer[Unit] =
        let* ans = find();
        if (ans) {
            let* s = evaluate(eval);
            appendReplacement!(s)
        } else
            fail("replaceStep!")

    /// `cond` must not perform a rewrite step.
    def ifThenElse(cond: Replacer[s], thenm: s -> Replacer[t], elsem: Replacer[t]): Replacer[t] =
        Replacer((sb, m, n) ->
            match apply1(cond, sb, m, n) {
                case Ok((a, n1)) => apply1(thenm(a), sb, m, n1)
                case Err(_) => apply1(elsem, sb, m, n)
            }
        )

    pub def done(): Replacer[Bool] = ifThenElse(content(), _ -> return(false), return(true))

    pub def setRegion!(start: Int32, end: Int32): Replacer[Unit] =
        liftPrim(x -> Text/Regex/Internal/PrimMatcher.region(x, start, end))

    pub def setPattern!(patt: Pattern): Replacer[Unit] =
        let Pattern(p1) = patt;
        liftPrim(x -> Ok(Text/Regex/Internal/PrimMatcher.usePattern(x, p1)))

    pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, ma: Replacer[a]): Replacer[ac] =
        if (n <= 0)
            return(acc)
        else {
            let* a = ma;
            countWith(append, append(a, acc), n-1, ma)
        }

    pub def count(n: Int32, rep: Replacer[a]): Replacer[List[a]] =
        use Data/Transient/DList.{empty, snoc, toList};
        map(toList, countWith((x, xs) -> snoc(xs, x), empty(), n, rep))



    /// Strategies

    pub def rewriteAll!(fn: String -> String): Replacer[Unit] =
        let* more = find();
        if (more) {
            let* s = content();
            let* _ = appendReplacement!(fn(s));
            rewriteAll!(fn)
        } else
            appendTail!()

    pub def evalRewriteAll!(eval: MatchEvaluator[String]): Replacer[Unit] =
        let* more = find();
        if (more) {
            let* s = evaluate(eval);
            let* _ = appendReplacement!(s);
            evalRewriteAll!(eval)
        } else
            appendTail!()

    pub def rewriteSome!(fn: String -> Option[String]): Replacer[Unit] =
        let* more = find();
        if (more) {
            let* s = content();
            let* _ = match fn(s) {
                        case Some(s1) => appendReplacement!(s1)
                        case None => return()
                    };
            rewriteSome!(fn)
        } else
            appendTail!()

    pub def evalRewriteSome!(eval: MatchEvaluator[Option[String]]): Replacer[Unit] =
        let* more = find();
        if (more) {
            let* opt = evaluate(eval);
            let* _ = match opt {
                        case Some(s) => appendReplacement!(s)
                        case None => return()
                    };
            evalRewriteSome!(eval)
        } else
            appendTail!()

    pub def rewriteAllWithIndex!(fn: (Int32, String) -> String): Replacer[Unit] =
        let* more = find();
        if (more) {
            let* ix = getIndex();
            let* s = content();
            let* _ = appendReplacement!(fn(ix, s));
            rewriteAllWithIndex!(fn)
        } else
            appendTail!()


    pub def evalRewriteAllWithIndex!(eval: Int32 -> MatchEvaluator[String]): Replacer[Unit] =
        let* more = find();
        if (more) {
            let* ix = getIndex();
            let* s = evaluate(eval(ix));
            let* _ = appendReplacement!(s);
            evalRewriteAllWithIndex!(eval)
        } else
            appendTail!()

    pub def rewriteSomeWithIndex!(fn: (Int32, String) -> Option[String]): Replacer[Unit] =
        let* more = find();
        if (more) {
            let* ix = getIndex();
            let* s = content();
            let* _ = match fn(ix, s) {
                        case Some(s1) => appendReplacement!(s1)
                        case None => return()
                    };
            rewriteSomeWithIndex!(fn)
        } else
            appendTail!()


    pub def evalRewriteSomeWithIndex!(eval: Int32 -> MatchEvaluator[Option[String]]): Replacer[Unit] =
        let* more = find();
        if (more) {
            let* ix = getIndex();
            let* opt = evaluate(eval(ix));
            let* _ = match opt {
                        case Some(s) => appendReplacement!(s)
                        case None => return()
                    };
            evalRewriteSomeWithIndex!(eval)
        } else
            appendTail!()



    pub def rewriteCount!(ntimes: Int32, fn: String -> String): Replacer[Unit] =
        if (ntimes < 0)
            return()
        else
            rewriteCountHelper!(ntimes, fn, 0)

    def rewriteCountHelper!(ntimes: Int32, fn: String -> String, i: Int32): Replacer[Unit] =
        let* more = find();
        if (more || i < ntimes) {
            let* s = content();
            let* _ = appendReplacement!(fn(s));
            rewriteCountHelper!(ntimes, fn, i + 1)
        } else
            appendTail!()

    pub def evalRewriteCount!(ntimes: Int32, eval: MatchEvaluator[String]): Replacer[Unit] =
        if (ntimes < 0)
            return()
        else
            evalRewriteCountHelper!(ntimes, eval, 0)

    def evalRewriteCountHelper!(ntimes: Int32, eval: MatchEvaluator[String], i: Int32): Replacer[Unit] =
        let* more = find();
        if (more || i < ntimes) {
            let* s = evaluate(eval);
            let* _ = appendReplacement!(s);
            evalRewriteCountHelper!(ntimes, eval, i + 1)
        } else
            appendTail!()


}        



