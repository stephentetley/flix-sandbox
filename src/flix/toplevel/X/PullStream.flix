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

///
/// The PullStream type.
///
/// A stream provides "demand driven" access to its elements - successive
/// elements are only produced when needed. Internally the stream is
/// represented as a generate function to produce the next element (and the
/// new stream state) and a reset function to reinitialize imperative streams.
///
pub opaque type PullStream[a] =
    { generate: Unit ~> (Unit ~> PullStream/Internal.Step[a])
    , reset: Unit ~> Unit
    }

namespace X/PullStream/Internal {

    ///
    /// The internal type of stream answers.
    /// `Skip` allows us to write `filter` and other functions that do not
    /// necessarily produce a value at each step.
    ///
    pub enum Step[a] {
        case Done,
        case Skip,
        case Yield(a)
    }

}


namespace X/PullStream {

    use X/PullStream/Internal.Step;
    use X/PullStream/Internal.Step.{Done, Skip, Yield};

    ///
    /// Returns the empty stream.
    ///
    pub def empty(): PullStream[a] =
        let gen = () -> {
            let dummy = ref 0;
            let next = () -> {
                dummy := 1;
                Done
            };
            next
        };
        PullStream({generate = gen, reset = _ -> () as & Impure})

    ///
    /// Helper function to extract generator.
    ///
    def getGenerator(s: PullStream[a]): Unit -> (Unit ~> Step[a]) & Impure =
        let PullStream(body) = s;
        body.generate


    ///
    /// Helper function to extract resetter.
    ///
    def getResetter(s: PullStream[a]): Unit -> Unit & Impure =
        let PullStream(body) = s;
        body.reset


    ///
    /// Truncate a stream but keep its resetter.
    ///
    def emptyOf(s: PullStream[a]): PullStream[a] =
        let gen = () -> {
            let dummy = ref 0;
            let next = () -> {
                dummy := 1;
                Done
            };
            next
        };
        PullStream({generate = gen, reset = getResetter(s)})

    ///
    /// Returns a one element stream containing `x`.
    ///
    pub def return(x: a): PullStream[a] =
        let gen = () -> {
            let n = ref 0;
            let next = () -> {
                if (deref n >= 0)
                    Done
                else {
                    n := 1 + deref n;
                    Yield(x)
                }
            };
            next
        };
        PullStream({generate = gen, reset = _ -> () as & Impure})

    ///
    /// Returns the stream produced by applying the mapping function produced by PullStream `sf` to the elements
    /// produced by PullStream `s`.
    ///    
    pub def ap(sf: PullStream[a -> b], s: PullStream[a]): PullStream[b] =
        zipWith((f,a) -> f(a), sf, s)


    pub def bind(s: PullStream[a], k: a -> PullStream[b]): PullStream[b] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () ->
                match sf() {
                    case Yield(a) => { 
                        let generate2 = getGenerator(k(a));
                        let sf2 = generate2();
                        sf2()
                    }
                    case Skip     => Skip
                    case Done     => Done
                };
            next
        };
        PullStream({ generate = gen, reset = getResetter(s) })

    pub def flatMap(k: a -> PullStream[b], s: PullStream[a]): PullStream[b] = bind(s, k)

    ///
    /// Construct an Iterator.
    ///
    /// The Iterator must provide three functions.
    ///
    /// * `hasNext` to test whether the iterator can still produce more values.
    ///
    /// * `next` to produce another value iff `hasNext` has indicated more values are available.
    ///
    /// * `finalize` to be called when the iterator has exhausted all its input. `finalize` should close
    ///    any open resources.
    ///
    type alias IteratorAlg[a] = { hasNext: Unit ~> Bool, next: Unit ~> a, resetter: Unit ~> Unit }

    /// Note: this implementation depletes imperative resources like file handles, this makes
    /// e.g. `length` a terminal operation, which it isn't for a "pure" stream.
    /// F# does not have this problem - imperative Seq's are replayable (is this due to reset?).
    ///
    /// Also F# is current / moveNext because that is what Java allows rather than hasNext / next
    /// which is the .net API.

    ///
    /// Returns a stream generated by an Iterator.
    ///
    pub def makePullStream(steps: IteratorAlg[a]): PullStream[a] =
        let hasNext = steps.hasNext;
        let iterNext = steps.next;
        let iterFinal = steps.resetter;
        let gen = () -> {
            let next = () ->
                if (not hasNext())
                    Done 
                else 
                    Yield(iterNext());
            next as & Impure
        };
        PullStream({ generate = gen, reset = iterFinal })


    ///
    /// Returns a stream of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty PullStream if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): PullStream[Int32] =
        if (b >= e)
            empty()
        else
            rangeHelper(b, e)

    ///
    /// Helper function for `range`.
    ///
    /// Precodition: b < e
    ///
    def rangeHelper(b: Int32, e: Int32): PullStream[Int32] =
        let gen = () -> {
            let rx = ref b;
            let next = () -> {
                let x = deref rx;
                if (x >= e)
                    Done
                else {
                    rx := x + 1;
                    Yield(x)
                }
            };
            next
        };
        PullStream({generate = gen, reset = _ -> () as & Impure})

    ///
    /// Returns the stream `s` as a list.
    ///
    pub def toList(s: PullStream[a]): List[a] & Impure =
        let generate1 = getGenerator(s);
        let reset1 = getResetter(s);
        let sf = generate1();
        let ans = toListHelper(sf, xs -> xs);
        reset1();
        ans

    ///
    /// Helper function for `toList
    ///
    def toListHelper(sf : Unit ~> Step[a], k: List[a] -> List[a]) : List[a] & Impure =
        match sf() {
            case Done     => k(Nil)
            case Skip     => toListHelper(sf, k)
            case Yield(x) => toListHelper(sf, xs -> k(x :: xs))
        }


    ///
    /// Applies `f` to every element of `s`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, s: PullStream[a]): Unit & Impure =
        let generate1 = getGenerator(s);
        let reset1 = getResetter(s);
        let sf = generate1();
        let ans = foreachHelper(f, sf);
        reset1();
        ans

    ///
    /// Helper function for `foreach`.
    ///
    def foreachHelper(f: a ~> Unit, sf : Unit ~> Step[a]): Unit & Impure =
        match sf() {
            case Done => ()
            case Skip => foreachHelper(f, sf)
            case Yield(a) => {
                f(a);
                foreachHelper(f, sf)
            }
        }

    ///
    /// Build a stream by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(a,st1)` to signal a new element `a` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the stream.
    ///
    pub def unfold(f: b -> Option[(a,b)], st: b): PullStream[a] =
        let gen = () -> {
            let state = ref st;
            let next = () ->
                match f(deref state) {
                    case None => Done
                    case Some(x,st2) => {
                        state := st2;
                        Yield(x)
                    }
                };
            next
        };
        PullStream({ generate = gen, reset = _ -> () as & Impure })

    ///
    /// Build a stream by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a new element `a`.
    ///
    /// `next` should return `None` to signal the end of building the stream.
    ///
    pub def unfoldWithIter(f: () ~> Option[a]): PullStream[a] =
        let gen = () -> {
            let next = () ->
                match f() {
                    case None => Done
                    case Some(x) => Yield(x)
                };
            next as & Impure
        };
        PullStream({ generate = gen, reset = _ -> () as & Impure })

    ///
    /// Returns the result of applying `f` to every element in PullStream `s`.
    ///
    pub def map(f: a -> b & ef, s: PullStream[a]): PullStream[b] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () ->
                match sf() {
                    case Yield(a) => {
                        let a1 = f(a) as & Impure;
                        Yield(a1)
                    }
                    case Skip     => Skip
                    case Done     => Done
                };
            next
        };
        PullStream({ generate = gen, reset = getResetter(s) })

    ///
    /// Returns the result of applying `f` to every element in PullStream `s` and the accumulator `ac`.
    ///
    pub def mapAccum(f: (ac, a) -> (ac, b), ac: ac, s: PullStream[a]): PullStream[b] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () -> {
                let accum = ref ac;
                match sf() {
                    case Yield(a) => {
                        let (ac1, b) = f(deref accum, a);
                        accum := ac1;
                        Yield(b)
                    }
                    case Skip     => Skip
                    case Done     => Done
                }
            };
            next
        };
        PullStream({ generate = gen, reset = getResetter(s) })

    ///
    /// Pair each element of stream `s` with its index, the index starts at `k`.
    ///
    pub def index(k: Int32, s: PullStream[a]): PullStream[(Int32, a)] =
        mapAccum((ix, a) -> (ix + 1, (ix, a)), k, s)

    ///
    /// Pair each element of stream `s` with its index, the index starts at 0.
    ///
    pub def zeroIndex(s: PullStream[a]): PullStream[(Int32, a)] =
        mapAccum((ix,a) -> (ix+1, (ix, a)), 0, s)

    ///
    /// Returns a stream built from the elements of list `xs`.
    ///
    pub def fromList(xs: List[a]): PullStream[a] =
        let gen = () -> {
            let rxs = ref xs;
            let next = () ->
                match (deref rxs) {
                    case Nil => Done
                    case x :: rs => {
                        rxs := rs;
                        Yield(x)
                    }
                };
            next
        };
        PullStream({ generate = gen, reset = _ -> () as & Impure })

    ///
    /// Returns a stream built from the elements of array `arr`.
    ///
    pub def fromArray(arr: Array[a]): PullStream[a] =
        let gen = () -> {
            let x = ref 0;
            let next = () -> {
                let ix = deref x;
                if (ix >= Array.length(arr))
                    Done
                else {
                    let p = Array.get(arr, ix);
                    x := ix + 1;
                    Yield(p)
                }
            };
            next
        };
        PullStream({ generate = gen, reset = _ -> () as & Impure })

    ///
    /// Add element `x` to the front of stream `s`.
    ///
    pub def cons(x: a, s: PullStream[a]) : PullStream[a] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let initial = ref true;
            let next = () ->
                if (deref initial != true)
                    sf()
                else {
                    initial := false;
                    Yield(x)
                };
            next
        };
        PullStream({ generate = gen, reset = getResetter(s) })

    ///
    /// Deconstruct the stream `s`.
    ///
    /// If the stream is productive return `Some(a,rest)` where `a` is the first element
    /// and `rest` is the rest of the stream.
    ///
    /// If the stream is exhausted return `None`.
    ///
    pub def view(s: PullStream[a]): Option[(a, PullStream[a])] & Impure =
        let generate1 = getGenerator(s);
        let reset1 = getResetter(s);
        let sf = generate1();
        viewHelper(sf, reset1)

    ///
    /// Helper function for `view`.
    ///
    def viewHelper(sf: Unit ~> Step[a], resetter: Unit ~> Unit): Option[(a, PullStream[a])] & Impure =
        match sf() {
            case Done => { resetter(); None }
            case Skip => viewHelper(sf, resetter)
            case Yield(a) => {
                let tail =
                    PullStream({
                        generate = () ->
                            let next = () -> sf();
                            next as & Impure
                        ,
                        reset = () -> resetter()
                     });
                Some(a, tail)
            }
        }

    ///
    /// Returns the head of `s`.
    ///
    /// If the stream is productive return `Some(a)` where `a` is the first element
    /// of `s`.
    ///
    /// If the stream is exhausted return `None`.
    ///
    pub def head(s: PullStream[a]): Option[a] & Impure =
        let generate1 = getGenerator(s);
        let sf = generate1();
        headHelper(sf)

    ///
    /// Helper function for `head`.
    ///
    def headHelper(sf: Unit ~> Step[a]): Option[a] & Impure =
        match sf() {
            case Done => None
            case Skip => headHelper(sf)
            case Yield(a) => Some(a)
        }



    ///
    /// Returns a stream where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `s1` and `b` is the element at index `i` in `s2`.
    ///
    /// If either `s1` or `s2` becomes depleted, then no further elements are added to the resulting stream.
    ///
    pub def zip(s1: PullStream[a], s2: PullStream[b]): PullStream[(a,b)] = zipWith((a,b) -> (a,b), s1, s2)

    ///
    /// Returns a stream where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `s1` and `b` is the element at index `i` in `s2`.
    ///
    /// If either `s1` or `s2` becomes depleted, then no further elements are added to the resulting stream.
    ///
    pub def zipWith(f: (a,b) -> c, s1: PullStream[a], s2: PullStream[b]): PullStream[c] =
        let gen = () -> {
            let generate1 = getGenerator(s1);
            let generate2 = getGenerator(s2);
            let sf1 = generate1();
            let sf2 = generate2();
            let next = () -> zipWithHelper(f, sf1, sf2);
            next as & Impure
        };
        let resetter = () -> {
            let reset1 = getResetter(s1);
            let reset2 = getResetter(s2);
            reset1();
            reset2()
        };
        PullStream({generate = gen, reset = resetter})

    ///
    /// Helper function for `zipWith`.
    ///
    /// Any case with `Done` is `Done`.
    ///
    def zipWithHelper(f: (a,b) -> c, sf1: Unit ~> Step[a], sf2: Unit ~> Step[b]) : Step[c] & Impure =
        match (sf1(), sf2()) {
            case (Yield(a), Yield(b))   => Yield(f(a,b))
            case (Yield(a), Skip)       => zipWithHelperGoRight(f, a, sf2)
            case (Skip, Yield(b))       => zipWithHelperGoLeft(f, sf1, b)
            case (Skip, Skip)           => Skip
            case (_, _)                 => Done
        }

    ///
    /// Helper function for `zipWith`.
    ///
    /// Precondition: stream `s1` (left) has yielded the value `x` but `s2` (right) yielded a `Skip`.
    /// Go along s2 (right) to produce a value.
    ///
    def zipWithHelperGoRight(f: (a,b) -> c, x: a, sf: Unit ~> Step[b]) : Step[c] & Impure =
        match sf() {
            case Yield(y)   => Yield(f(x,y))
            case Done       => Done
            case Skip       => zipWithHelperGoRight(f, x, sf)
        }

    ///
    /// Helper function for `zipWith`.
    ///
    /// Precondition: stream `s2` (right) has yielded the value `x` but `s1` (left) yielded a `Skip`.
    /// Go along s2 (left) to produce a value.
    ///
    def zipWithHelperGoLeft(f: (a,b) -> c, sf: Unit ~> Step[a], y: b) : Step[c] & Impure =
        match sf() {
            case Yield(x)   => Yield(f(x,y))
            case Done       => Done
            case Skip       => zipWithHelperGoLeft(f, sf, y)
        }

    ///
    /// Applies `f` to a start value `ac` and all elements in `s` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def fold(f: (b, a) -> b & e, ac: b, s: PullStream[a]): b & Impure =
        let generate1 = getGenerator(s);
        let reset1 = getResetter(s);
        let sf = generate1();
        let ans = foldHelper(f, sf, ac);
        reset1();
        ans


    ///
    /// Helper function for `fold`.
    ///
    def foldHelper(f: (b,a) -> b & e, sf : Unit ~> Step[a], ac: b): b & Impure =
        match sf() {
            case Done => ac
            case Skip => foldHelper(f, sf, ac)
            case Yield(a) =>
                foldHelper(f, sf, f(ac,a) as & Impure)
        }

    ///
    /// Returns `s2` appended to `s1`.
    ///
    pub def append(s1: PullStream[a], s2: PullStream[a]): PullStream[a] =
        let gen = () -> {
            let generate1 = getGenerator(s1);
            let generate2 = getGenerator(s2);
            let sf1 = generate1();
            let sf2 = generate2();
            let from1 = ref true;
            let next = () ->
                if (deref from1 == false)
                    sf2()
                else
                    match sf1() {
                        case Skip => Skip
                        case Yield(a) => Yield(a)
                        case Done => {
                            from1 := false;
                            Skip
                        }
                    };
            next
        };
        let resetter = () -> {
            let reset1 = getResetter(s1);
            let reset2 = getResetter(s2);
            reset1();
            reset2()
        };
        PullStream({generate = gen, reset = resetter})

    ///
    /// Returns the first `n` elements of stream `s`.
    ///
    /// Returns `s` if `n > length(s)`.
    /// Returns the empty stream if `n < 0`.
    ///
    pub def take(n: Int32, s: PullStream[a]): PullStream[a] =
        if (n <= 0)
            emptyOf(s)
        else
            takeHelper(n, s)

    ///
    /// Helper function for `take`.
    ///
    /// Precondition: n > 0 .
    ///
    def takeHelper(n: Int32, s: PullStream[a]): PullStream[a] =
        let gen = () -> {
            let x = ref 0;
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () ->
                if (deref x >= n)
                    Done
                else {
                    match sf() {
                        case Yield(a) => {
                            x := deref x + 1;
                            Yield(a)
                        }
                        case Skip => Skip
                        case Done => Done
                    }
                };
            next
        };
        PullStream({ generate = gen, reset = getResetter(s) })


    ///
    /// Returns the longest prefix of `s` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhile(f: a -> Bool, s: PullStream[a]): PullStream[a] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () ->
                match sf() {
                    case Yield(a) =>
                        if (f(a) == false)
                            Done
                        else
                            Yield(a)
                    case Skip => Skip
                    case Done => Done
                };
            next
        };
        PullStream({ generate = gen, reset = getResetter(s) })


    ///
    /// Returns `s` without the first `n` elements.
    ///
    /// Returns the empty stream if `n > length(xs)`.
    /// Returns `xs` if `n < 0`.
    ///
    pub def drop(n: Int32, s: PullStream[a]): PullStream[a] =
        if (n <= 0)
            s
        else
            dropHelper(n, s)

    ///
    /// Helper function for `drop`.
    ///
    /// Precondition: n >= 0 .
    ///
    def dropHelper(n: Int32, s: PullStream[a]): PullStream[a] =
        let gen = () -> {
            let x = ref n;
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () -> match sf() {
                case Yield(a) =>
                    if (deref x == 0)
                        Yield(a)
                    else {
                        x := deref x - 1;
                        Skip
                    }
                case Skip => Skip
                case Done => Done
                };
            next
        };
        PullStream({ generate = gen, reset = getResetter(s) })

    ///
    /// Returns `s` without the longest prefix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhile(f: a -> Bool, s: PullStream[a]): PullStream[a] =
        let gen = () -> {
            let afterPrefix = ref false;
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () ->
                match sf() {
                    case Yield(a) =>
                        if (deref afterPrefix == true)
                            Yield(a)
                        else
                            if (f(a) == true)
                                Skip
                            else {
                                afterPrefix := true;
                                Yield(a)
                            }
                    case Skip => Skip
                    case Done => Done
                };
            next
        };
        PullStream({ generate = gen, reset = getResetter(s) })

    ///
    /// Returns a stream of every element in `s` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool, s: PullStream[a]): PullStream[a] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () ->
                match sf() {
                    case Done => Done
                    case Skip => Skip
                    case Yield(a) =>
                        if (f(a) == false)
                            Skip
                        else
                            Yield(a)
                };
            next
        };
        PullStream({ generate = gen, reset = getResetter(s) })

    ///
    /// Returns a stream of every element in `s` where `f` produces a value `Some(x)`.
    ///
    pub def choose(f: a -> Option[b] & e, s: PullStream[a]): PullStream[b] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () ->
                match sf() {
                    case Done => Done
                    case Skip => Skip
                    case Yield(a) => {
                        let ans = f(a) as & Impure;
                        match ans {
                            case Some(b) => Yield(b)
                            case None => Skip
                        }
                    }
                };
            next
        };
        PullStream({ generate = gen, reset = getResetter(s) })

    ///
    /// Returns `true` if streams `s1` and `s1` have the same elements, i.e. are equivalent.
    ///
    pub def sameElements[a](s1: PullStream[a], s2: PullStream[a]): Bool & Impure with Eq[a] =
        let generate1 = getGenerator(s1);
        let generate2 = getGenerator(s2);
        let reset1 = getResetter(s1);
        let reset2 = getResetter(s2);
        let sf1 = generate1();
        let sf2 = generate2();
        let ans = sameElementsHelper(sf1, sf2);
        reset1();
        reset2();
        ans


    ///
    /// Helper function for `sameElements`.
    ///
    def sameElementsHelper[a](sf1: Unit ~> Step[a], sf2: Unit ~> Step[a]): Bool & Impure with Eq[a] =
        match (sf1(), sf2()) {
            case (Yield(a), Yield(b))   => if (a != b) false else sameElementsHelper(sf1, sf2)
            case (Yield(a), Skip)       => sameElementsHelperGoRight(a, sf1, sf2)
            case (Skip, Yield(b))       => sameElementsHelperGoLeft(sf1, b, sf2)
            case (Yield(_), Done)       => false
            case (Done, Yield(_))       => false
            case (Skip, Skip)           => sameElementsHelper(sf1, sf2)
            case (Skip, Done)           => sameElementsHelper(sf1, sf2)
            case (Done, Skip)           => sameElementsHelper(sf1, sf2)
            case (Done, Done)           => true
        }

    ///
    /// Helper function for `sameElements`.
    ///
    /// Precondition: stream `sf1` (left) has yielded the value `x` but `sf2` (right) yielded a `Skip`.
    /// Go along sf2 (right) to produce a value, if same continue along both.
    ///
    def sameElementsHelperGoRight[a](x: a, sf1: Unit ~> Step[a], sf2: Unit ~> Step[a]) : Bool & Impure with Eq[a] =
        match sf2() {
            case Yield(y)   => if (x != y) false else sameElementsHelper(sf1, sf2)
            case Done       => false
            case Skip       => sameElementsHelperGoRight(x, sf1, sf2)
        }

    ///
    /// Helper function for `sameElements`.
    ///
    /// Precondition: stream `sf2` (right) has yielded the value `x` but `sf1` (left) yielded a `Skip`.
    /// Go along sf2 (left) to produce a value, if same continue along both.
    ///
    def sameElementsHelperGoLeft[a](sf1: Unit ~> Step[a], y: a, sf2: Unit ~> Step[a]) : Bool & Impure with Eq[a] =
        match sf1() {
            case Yield(x)   => if (x != y) false else sameElementsHelper(sf1, sf2)
            case Done       => true
            case Skip       => sameElementsHelperGoLeft(sf1, y, sf2)
        }

    ///
    /// Returns the substream of `xs` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// That is, an element at index `i` in `xs` is part of the returned sublist if and only if `i >= b` and `i < e`.
    /// Note: Indices that are out of bounds in `xs` are not considered (i.e. slice(b, e, xs) = slice(max(0,b), min(length(xs),e), xs)).
    ///
    pub def slice(b: Int32, e: Int32, s: PullStream[a]): PullStream[a] =
        if (b >= e)
            emptyOf(s)
        else
            sliceHelper(b, e, s)

    ///
    /// Helper function for `slice`.
    ///
    /// Preconditions: b >= 0 && b < e
    ///
    pub def sliceHelper(b: Int32, e: Int32, s: PullStream[a]): PullStream[a] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let ix = ref 0;
            let next = () ->
                match sf() {
                    case Done => Done
                    case Skip => Skip
                    case Yield(a) => {
                        let i = deref ix;
                        if (i < b or i >= e) {
                            ix := i + 1;
                            Skip
                        } else {
                            ix := i + 1;
                            Yield(a)
                        }
                    }
                };
            next
        };
        PullStream({generate = gen, reset = getResetter(s)})


    ///
    /// Returns a stream with the element `a` repeated `n` times.
    ///
    /// Returns the empty stream if `n < 0`.
    ///
    pub def repeat(a: a, n: Int32): PullStream[a] =
        if (n <= 0)
            empty()
        else repeatHelper(a, n)

    ///
    /// Helper function for `repeat`.
    ///
    def repeatHelper(a: a, n: Int32): PullStream[a] =
        let gen = () -> {
            let ix = ref 0;
            let next = () ->
                if (deref ix >= n)
                    Done
                else {
                    ix := deref ix + 1;
                    Yield(a)
                };
            next
        };
        PullStream({ generate = gen, reset = _ -> () as & Impure })

    ///
    /// Applies `f` to a start value `ac` and all `Ok` elements in `s` going from left to right.
    /// If an `Err` is encountered the stream closed and the error is returned.
    ///
    pub def foldResult(f: (b, a) -> Result[b, e] & ef, ac: b, s: PullStream[a]): Result[b, e] & Impure =
        let generate1 = getGenerator(s);
        let reset1 = getResetter(s);
        let sf = generate1();
        let ans = foldResultHelper(f, sf, ac);
        reset1();
        ans

    ///
    /// Helper function for `foldResult`.
    ///
    def foldResultHelper(f: (b, a) -> Result[b, err] & ef, sf: Unit ~> Step[a], ac: b): Result[b, err] & Impure =
        match sf() {
            case Done => Ok(ac)
            case Skip => foldResultHelper(f, sf, ac)
            case Yield(a) => {
                let ans = f(ac, a) as & Impure;
                match ans {
                    case Err(err) => Err(err)
                    case Ok(ac1) => foldResultHelper(f, sf, ac1)
                }
            }
        }

    ///
    /// ...Collects all Somes, not short-circuiting...
    ///
    pub def chooseToList(f: a -> Option[b] & e, s: PullStream[a]): List[b] & Impure =
        let generate1 = getGenerator(s);
        let reset1 = getResetter(s);
        let sf = generate1();
        let ans = chooseToListHelper(f, sf, ks -> ks as & Impure);
        reset1();
        ans

    ///
    /// Helper function for `chooseToList`.
    ///
    def chooseToListHelper(f: a -> Option[b] & f, sf: Unit ~> Step[a], k: List[b] ~> List[b]): List[b] & Impure =
        match sf() {
            case Done => k(Nil)
            case Skip => chooseToListHelper(f, sf, k)
            case Yield(a) => match f(a) {
                case None => chooseToListHelper(f, sf, k)
                case Some(x) => chooseToListHelper(f, sf, ks -> k(x :: ks))
            }
        }

    ///
    /// ...Multiple results at each step, not short-circuiting...
    ///
    pub def collectToList(f: a -> List[b] & e, s: PullStream[a]): List[b] & Impure =
        let generate1 = getGenerator(s);
        let reset1 = getResetter(s);
        let sf = generate1();
        let ans = collectToListHelper(f, sf, ks -> ks as & Impure);
        reset1();
        ans

    ///
    /// Helper function for `collectToList`.
    ///
    def collectToListHelper(f: a -> List[b] & f, sf: Unit ~> Step[a], k: List[b] ~> List[b]): List[b] & Impure =
        match sf() {
            case Done => k(Nil)
            case Skip => collectToListHelper(f, sf, k)
            case Yield(a) => {
                let xs = f(a);
                collectToListHelper(f, sf, ks -> k(xs ::: ks))
            }
        }


    ///
    /// Short-circuiting, fails on first None...
    ///
    pub def traverseToListOption(f: a -> Option[b] & e, s: PullStream[a]): Option[List[b]] & Impure =
        let generate1 = getGenerator(s);
        let reset1 = getResetter(s);
        let sf = generate1();
        let ans = traverseToListOptionHelper(f, sf, _ -> None as & Impure,  ks -> Some(ks) as & Impure);
        reset1();
        ans

    ///
    /// Helper function for `traverseToListOption`.
    ///
    def traverseToListOptionHelper(f: a -> Option[b] & f, sf: Unit ~> Step[a], fk: Unit ~> Option[List[b]], sk: List[b] ~> Option[List[b]]): Option[List[b]] & Impure =
        match sf() {
            case Done => sk(Nil)
            case Skip => traverseToListOptionHelper(f, sf, fk, sk)
            case Yield(a) => match f(a) {
                case None => fk()
                case Some(x) => traverseToListOptionHelper(f, sf, fk, ks -> sk(x :: ks))
            }
        }

    ///
    /// Short-circuiting, fails on first Err...
    ///
    pub def traverseToListResult(f: a -> Result[b, e] & f, s: PullStream[a]): Result[List[b], e] & Impure =
        let generate1 = getGenerator(s);
        let reset1 = getResetter(s);
        let sf = generate1();
        let ans = traverseToListResultHelper(f, sf, e -> Err(e) as & Impure,  ks -> Ok(ks) as & Impure);
        reset1();
        ans

    ///
    /// Helper function for `traverseToListResult`.
    ///
    def traverseToListResultHelper(f: a -> Result[b, e] & f, sf: Unit ~> Step[a], fk: e ~> Result[List[b], e], sk: List[b] ~> Result[List[b], e]): Result[List[b], e] & Impure =
        match sf() {
            case Done => sk(Nil)
            case Skip => traverseToListResultHelper(f, sf, fk, sk)
            case Yield(a) => match f(a) {
                case Err(err) => fk(err)
                case Ok(x) => traverseToListResultHelper(f, sf, fk, ks -> sk(x :: ks))
            }
        }


    ///
    /// Applies `f` to every element of `s`.
    ///
    /// The function `f` must be impure.
    /// Fails on first error.
    ///
    pub def foreachResult(f: a ~> Result[b, e], s: PullStream[a]): Result[Unit, e] & Impure =
        let generate1 = getGenerator(s);
        let reset1 = getResetter(s);
        let sf = generate1();
        let ans = foreachResultHelper(f, sf);
        reset1();
        ans

    ///
    /// Helper function for `foreachResult`.
    ///
    def foreachResultHelper(f: a ~> Result[b, e], sf : Unit ~> Step[a]): Result[Unit, e] & Impure =
        match sf() {
            case Done => Ok()
            case Skip => foreachResultHelper(f, sf)
            case Yield(a) => match f(a) {
                case Ok(_) => foreachResultHelper(f, sf)
                case Err(e) => Err(e)
            }
        }

    ///
    /// Returns the sum of the elements of Stream `s`.
    ///
    /// `s` must be finite.
    ///
    pub def sum(s: PullStream[Int32]): Int32 & Impure =
        fold((a, i) -> a + i, 0, s)

    ///
    /// Returns the length of Stream `s`.
    ///
    /// `s` must be finite.
    ///
    pub def length(s: PullStream[a]): Int32 & Impure =
        fold((a, _) -> a + 1, 0, s)


    ///
    /// Returns the average of the elements of Stream `s`.
    ///
    /// `s` must be finite.
    ///
    pub def average(s: PullStream[Float64]): Float64 & Impure =
        let (n, x) = fold((xy, d) -> {let (i, tot) = xy; (i + 1i64, tot + d)}, (0i64, 0.0f64), s);
        x / Int64.toFloat64(n)

    ///
    /// Optionally finds the largest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    /// `s` must be finite.
    ///
    pub def maximum[a](s: PullStream[a]): Option[a] & Impure with Order[a] =
        use Option.{flatMap};
        let* (x1, s1)   = view(s);
        let max         = fold((a, x) -> Order.max(a, x), x1, s1);
        Some(max)

    ///
    /// Returns `true` if and only if `xs` contains the element `a`.
    ///
    // pub def memberOf[a : Eq](a: a, s: PullStream[a]): Bool & Impure = match xs {
    //     case Nil => false
    //     case x :: rs => if (x == a) true else memberOf(a, rs)
    // }


}
