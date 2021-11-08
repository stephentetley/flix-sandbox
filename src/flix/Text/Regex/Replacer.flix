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



/// This is a "steppable" replacer like Matcher.
/// Index is number of `finds` not the number of replacements.

namespace Text/Regex {

    pub opaque type Replacer[a] = (StringBuilder, ##java.util.regex.Matcher, Int32) -> Result[(a, Int32), String]

    instance Functor[Replacer] {
        pub def map(f: a -> b & ef, o: Replacer[a]): Replacer[b] & ef = Text/Regex/Replacer.map(f, o)
    }

    instance Applicative[Replacer] {
        pub def point(x: a) : Replacer[a] = Text/Regex/Replacer.point(x)

        pub def ap(f: Replacer[a -> b & ef], x: Replacer[a]) : Replacer[b] & ef = Text/Regex/Replacer.ap(f, x)
    }

    instance Monad[Replacer] {
        pub def flatMap(f: a -> Replacer[b] & ef, x: Replacer[a]) : Replacer[b] & ef = Text/Regex/Replacer.flatMap(f, x)
    }

}

namespace Text/Regex/Replacer {

    use Text/Regex.{Replacer};
    use Text/Regex.Replacer.{Replacer};

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};
    use Text/Regex.{MatchEvaluator};
    use Text/Regex.MatchEvaluator.{MatchEvaluator};

    pub def runReplacer(rep: Replacer[a], patt: Pattern, input: String): Result[(a, String), String] & Impure =
        use Result.flatMap;
        let sb = StringBuilder.new();
        let Replacer(rep1) = rep;
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input);
        /// Start index at -1 because we haven't found anything yet...
        match rep1(sb, m1, -1) {
            case Ok(a, _) => {
                let s = StringBuilder.toString(sb);
                Ok((a, s))
            }
            case Err(e) => Err(e)
        }

    pub def execReplacer(rep: Replacer[a], patt: Pattern, input: String): Result[String, String] & Impure =
        runReplacer(rep, patt, input) |> Result.map(snd)

    ///
    /// Version of `runReplacer` that takes a regex string and compiles it to a Pattern.
    ///
    pub def runsReplacer(rep: Replacer[a], regex: String, input: String): Result[(a, String), String] & Impure =
        use Result.flatMap;
        let* patt = Text/Regex/Pattern.compile(regex) |> Result.mapErr(constant("runsReplacer - invalid regex: ${regex}"));
        runReplacer(rep, patt, input)

    pub def execsReplacer(rep: Replacer[a], regex: String, input: String): Result[String, String] & Impure =
        runsReplacer(rep, regex, input) |> Result.map(snd)

    pub def point(x: a): Replacer[a] =
        Replacer((_, _, n) -> Ok(x, n))


    pub def throwError(msg: String): Replacer[a] =
        Replacer((_, _, _) -> Err(msg))

    def apply1(rep: Replacer[a], sb: StringBuilder, m: ##java.util.regex.Matcher, n: Int32): Result[(a, Int32), String] =
        let Replacer(f) = rep;
        f(sb, m, n)

    pub def map(f: a -> b & ef, rep: Replacer[a]): Replacer[b] & ef =
        Replacer((sb, m, n) ->
            match apply1(rep, sb, m, n) {
                case Ok((a, n1)) => {
                    let a1 = f(a) as & Pure;
                    Ok(a1, n1)
                }
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def ap(repf: Replacer[a -> b & ef], rep: Replacer[a]): Replacer[b] & ef =
        Replacer((sb, m, n) ->
            match apply1(repf, sb, m, n) {
                case Ok((f, n1)) => match apply1(rep, sb, m, n1) {
                    case Ok((a, n2)) => Ok((f(a) as & Pure, n2))
                    case Err(e2) => Err(e2)
                }
                case Err(e1) => Err(e1)
            }
        ) as & ef

    pub def bind(rep: Replacer[a], k: a -> Replacer[b] & ef): Replacer[b] & ef =
        Replacer((sb, m, n) ->
            match apply1(rep, sb, m, n) {
                case Ok((a, n1)) => apply1(k(a) as & Pure, sb , m, n1)
                case Err(e) => Err(e)
            }
        ) as & ef

    pub def flatMap(k: a -> Replacer[b] & ef, x: Replacer[a]): Replacer[b] & ef = bind(x, k)

    pub def ignore(x: Replacer[a]): Replacer[Unit] = map(_ -> (), x)


    def liftPrim(fn: ##java.util.regex.Matcher ~> a): Replacer[a] & Impure =
        Replacer((_, m, n) -> Ok((fn(m) as & Pure, n))) as & Impure

    def liftPrimResult(fn: ##java.util.regex.Matcher ~> Result[a, String]): Replacer[a] & Impure =
        Replacer((_, m, n) -> Result.map(x -> (x, n), fn(m) as & Pure)) as & Impure

    def liftWriterPrim(fn : (##java.util.regex.Matcher, StringBuilder) ~> a): Replacer[a] & Impure =
        Replacer((sb, m, n) -> Ok((fn(m, sb) as & Pure, n))) as & Impure

    def liftWriterPrimResult(fn : (##java.util.regex.Matcher, StringBuilder) ~> Result[a, String]): Replacer[a] & Impure =
        Replacer((sb, m, n) -> Result.map(x -> (x, n), fn(m, sb) as & Pure)) as & Impure

    pub def appendTail!(): Replacer[Unit] & Impure =
        use Text/Regex/Internal/Matcher.{appendTail};
        liftWriterPrim((m, sb) -> appendTail(m, sb))

    pub def appendReplacement!(rep: String): Replacer[Unit] & Impure =
        use Text/Regex/Internal/Matcher.{appendReplacement};
        liftWriterPrimResult((m, sb) -> appendReplacement(m, sb, rep))

    pub def getIndex(): Replacer[Int32] =
        Replacer((_, _, n) -> Ok((n, n)))

    def incrementFinds!(incr: Bool): Replacer[Unit] =
        Replacer((_, _, n) -> Ok(((), if (incr) n + 1 else n)))

    pub def find(): Replacer[Bool] & Impure =
        let* bfound = liftPrim(x -> Text/Regex/Internal/Matcher.find(x));
        let* _ = incrementFinds!(bfound);
        point(bfound)

    pub def findFrom(start: Int32): Replacer[Bool] & Impure =
        let* bfound = liftPrimResult(x -> Text/Regex/Internal/Matcher.findFrom(x, start));
        let* _ = incrementFinds!(bfound);
        point(bfound)

    pub def content(): Replacer[String] & Impure = liftPrimResult(Text/Regex/Internal/Matcher.group)

    pub def evaluate(eval: MatchEvaluator[a]): Replacer[a] & Impure =
        let MatchEvaluator(fn) = eval;
        liftPrimResult(x -> fn(x) as & Impure)

    pub def replaceStep!(fn: String -> String): Replacer[Unit] & Impure =
        let* ans = find();
        if (ans) {
            let* s = content();
            appendReplacement!(fn(s))
        } else
            throwError("replaceStep!")

    pub def evaluateReplace!(eval: MatchEvaluator[String]): Replacer[Unit] & Impure =
        let* ans = find();
        if (ans) {
            let* s = evaluate(eval);
            appendReplacement!(s)
        } else
            throwError("replaceStep!")

    /// `cond` must not perform a rewrite step.
    def ifThenElse(cond: Replacer[s], thenm: s -> Replacer[t], elsem: Replacer[t]): Replacer[t] =
        Replacer((sb, m, n) ->
            match apply1(cond, sb, m, n) {
                case Ok((a, n1)) => apply1(thenm(a), sb, m, n1)
                case Err(_) => apply1(elsem, sb, m, n)
            }
        )

    pub def done(): Replacer[Bool] & Impure = ifThenElse(content(), _ -> point(false), point(true))

    pub def setRegion!(start: Int32, end: Int32): Replacer[Unit] & Impure =
        liftPrimResult(x -> Text/Regex/Internal/Matcher.region(x, start, end))

    pub def setPattern!(patt: Pattern): Replacer[Unit] & Impure =
        let Pattern(patt1) = patt;
        liftPrimResult(x -> Text/Regex/Internal/Matcher.usePattern(x, patt1))

    pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, ma: Replacer[a]): Replacer[ac] =
        if (n <= 0)
            point(acc)
        else {
            let* a = ma;
            countWith(append, append(a, acc), n-1, ma)
        }

    pub def count(n: Int32, rep: Replacer[a]): Replacer[List[a]] =
        use Chain.{empty, snoc, toList};
        map(toList, countWith((x, xs) -> snoc(xs, x), empty(), n, rep))



    /// Strategies

    pub def rewriteAll!(fn: String -> String): Replacer[Unit] & Impure =
        let* more = find();
        if (more) {
            let* s = content();
            let* _ = appendReplacement!(fn(s));
            rewriteAll!(fn)
        } else
            appendTail!()

    pub def evalRewriteAll!(eval: MatchEvaluator[String]): Replacer[Unit] & Impure =
        let* more = find();
        if (more) {
            let* s = evaluate(eval);
            let* _ = appendReplacement!(s);
            evalRewriteAll!(eval)
        } else
            appendTail!()

    pub def rewriteSome!(fn: String -> Option[String]): Replacer[Unit] & Impure =
        let* more = find();
        if (more) {
            let* s = content();
            let* _ = match fn(s) {
                        case Some(s1) => appendReplacement!(s1)
                        case None => point()
                    };
            rewriteSome!(fn)
        } else
            appendTail!()

    pub def evalRewriteSome!(eval: MatchEvaluator[Option[String]]): Replacer[Unit] & Impure =
        let* more = find();
        if (more) {
            let* opt = evaluate(eval);
            let* _ = match opt {
                        case Some(s) => appendReplacement!(s)
                        case None => point()
                    };
            evalRewriteSome!(eval)
        } else
            appendTail!()

    pub def rewriteAllWithIndex!(fn: (Int32, String) -> String): Replacer[Unit] & Impure =
        let* more = find();
        if (more) {
            let* ix = getIndex();
            let* s = content();
            let* _ = appendReplacement!(fn(ix, s));
            rewriteAllWithIndex!(fn)
        } else
            appendTail!()


    pub def evalRewriteAllWithIndex!(eval: Int32 -> MatchEvaluator[String]): Replacer[Unit] & Impure =
        let* more = find();
        if (more) {
            let* ix = getIndex();
            let* s = evaluate(eval(ix));
            let* _ = appendReplacement!(s);
            evalRewriteAllWithIndex!(eval)
        } else
            appendTail!()

    pub def rewriteSomeWithIndex!(fn: (Int32, String) -> Option[String]): Replacer[Unit] & Impure =
        let* more = find();
        if (more) {
            let* ix = getIndex();
            let* s = content();
            let* _ = match fn(ix, s) {
                        case Some(s1) => appendReplacement!(s1)
                        case None => point()
                    };
            rewriteSomeWithIndex!(fn)
        } else
            appendTail!()


    pub def evalRewriteSomeWithIndex!(eval: Int32 -> MatchEvaluator[Option[String]]): Replacer[Unit] & Impure =
        let* more = find();
        if (more) {
            let* ix = getIndex();
            let* opt = evaluate(eval(ix));
            let* _ = match opt {
                        case Some(s) => appendReplacement!(s)
                        case None => point()
                    };
            evalRewriteSomeWithIndex!(eval)
        } else
            appendTail!()



    pub def rewriteCount!(ntimes: Int32, fn: String -> String): Replacer[Unit] & Impure =
        if (ntimes < 0)
            point()
        else
            rewriteCountHelper!(ntimes, fn, 0)

    def rewriteCountHelper!(ntimes: Int32, fn: String -> String, i: Int32): Replacer[Unit] & Impure =
        let* more = find();
        if (more or i < ntimes) {
            let* s = content();
            let* _ = appendReplacement!(fn(s));
            rewriteCountHelper!(ntimes, fn, i + 1)
        } else
            appendTail!()

    pub def evalRewriteCount!(ntimes: Int32, eval: MatchEvaluator[String]): Replacer[Unit] & Impure =
        if (ntimes < 0)
            point()
        else
            evalRewriteCountHelper!(ntimes, eval, 0)

    def evalRewriteCountHelper!(ntimes: Int32, eval: MatchEvaluator[String], i: Int32): Replacer[Unit] & Impure =
        let* more = find();
        if (more or i < ntimes) {
            let* s = evaluate(eval);
            let* _ = appendReplacement!(s);
            evalRewriteCountHelper!(ntimes, eval, i + 1)
        } else {
            appendTail!()
        }

}        



