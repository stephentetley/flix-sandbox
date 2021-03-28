/*
 * Copyright 2021 Stephen Tetley
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
/// The Cursor type.
///
/// An Cursor is a simple Stream-like object that provides "demand driven" access
/// to its elements - successive elements are only produced when needed.
/// Unlike a Stream, an Cursor is not replayable - accessing the stream
/// depletes it. Due to depletion many common List / replayable-Stream
/// functions cannot be implemented, or have to be implemented as so called
/// "terminal functions" which exhaust the input and stop any further processing.

pub opaque type Cursor[a] =
    { generate: Unit ~> (Unit ~> Cursor/Internal.Step[a])
    , finalize: Unit ~> Unit
    }

namespace Cursor/Internal {

    ///
    /// The internal type of iterator answers.
    /// `Skip` allows us to write `filter` and other functions that do not
    /// necessarily produce a value at each step.
    ///
    pub enum Step[a] {
        case Done,
        case Skip,
        case Yield(a)
    }

}


namespace Cursor {

    use Cursor/Internal.Step;
    use Cursor/Internal.Step.{Done, Skip, Yield};

    ///
    /// Returns the empty stream.
    ///
    pub def empty(): Cursor[a] =
        let gen = () -> {
            let dummy = ref 0;
            let next = () -> {
                dummy := 1;
                Done
            };
            next
        };
        Cursor({ generate = gen, finalize = _ -> () as & Impure })

    ///
    /// Helper function to extract finalizer.
    ///
    def getFinalizer(s: Cursor[a]): Unit -> Unit & Impure =
        let Cursor(body) = s;
        body.finalize

    ///
    /// Helper function to extract generator.
    ///
    def getGenerator(s: Cursor[a]): Unit -> (Unit ~> Step[a]) & Impure =
        let Cursor(body) = s;
        body.generate

    ///
    /// Truncate a stream but keep its finalizer.
    ///
    def emptyOf(s: Cursor[a]): Cursor[a] =
        let gen = () -> {
            let dummy = ref 0;
            let next = () -> {
                dummy := 1;
                Done
            };
            next
        };
        Cursor({ generate = gen, finalize = getFinalizer(s) })

    ///
    /// Returns a one element stream containing `x`.
    ///
    pub def return(x: a): Cursor[a] =
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
        Cursor({ generate = gen, finalize = _ -> () as & Impure })

    ///
    /// Returns the stream produced by applying the mapping function produced by Cursor `sf` to the elements
    /// produced by Cursor `s`.
    ///    
    pub def ap(sf: Cursor[a -> b], s: Cursor[a]): Cursor[b] =
        zipWith((f,a) -> f(a), sf, s)


    pub def bind(s: Cursor[a], k: a -> Cursor[b]): Cursor[b] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })

    pub def flatMap(k: a -> Cursor[b], s: Cursor[a]): Cursor[b] = bind(s, k)

    ///
    /// Construct an Cursor.
    ///
    /// The Cursor must provide three functions.
    ///
    /// * `hasNext` to test whether the iterator can still produce more values.
    ///
    /// * `next` to produce another value iff `hasNext` has indicated more values are available.
    ///
    /// * `finalize` to be called when the iterator has exhausted all its input. `finalize` should close
    ///    any open resources.
    ///
    type alias CursorAlg[a] = { hasNext: Unit ~> Bool, next: Unit ~> a, finalize: Unit ~> Unit }

    /// Note: this implementation depletes imperative resources like file handles, this makes
    /// e.g. `length` a terminal operation, which it isn't for a "pure" stream.
    /// F# does not have this problem - imperative Seq's are replayable (is this due to reset?).
    ///
    /// Also F# is current / moveNext because that is what Java allows rather than hasNext / next
    /// which is the .net API.

    ///
    /// Returns a stream generated by an Cursor.
    ///
    pub def makeCursor(steps: CursorAlg[a]): Cursor[a] =
        let hasNext = steps.hasNext;
        let iterNext = steps.next;
        let iterFinal = steps.finalize;
        let gen = () -> {
            let next = () ->
                if (not hasNext())
                    Done 
                else 
                    Yield(iterNext());
            next as & Impure
        };
        Cursor({generate = gen, finalize = iterFinal})


    ///
    /// Returns a stream of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty Cursor if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Cursor[Int32] =
        if (b >= e)
            empty()
        else
            rangeHelper(b, e)

    ///
    /// Helper function for `range`.
    ///
    /// Precodition: b < e
    ///
    def rangeHelper(b: Int32, e: Int32): Cursor[Int32] =
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
        Cursor({ generate = gen, finalize = _ -> () as & Impure })

    ///
    /// Returns the stream `s` as a list.
    ///
    pub def toList(s: Cursor[a]): List[a] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = toListHelper(sf, xs -> xs);
        finalize1();
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
    pub def foreach(f: a ~> Unit, s: Cursor[a]): Unit & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = foreachHelper(f, sf);
        finalize1();
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
    pub def unfold(f: b -> Option[(a,b)], st: b): Cursor[a] =
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
        Cursor({ generate = gen, finalize = _ -> () as & Impure })

    ///
    /// Build a stream by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a new element `a`.
    ///
    /// `next` should return `None` to signal the end of building the stream.
    ///
    pub def unfoldWithIter(f: () ~> Option[a]): Cursor[a] =
        let gen = () -> {
            let next = () ->
                match f() {
                    case None => Done
                    case Some(x) => Yield(x)
                };
            next as & Impure
        };
        Cursor({ generate = gen, finalize = _ -> () as & Impure })

    ///
    /// Returns the result of applying `f` to every element in Cursor `s`.
    ///
    pub def map(f: a -> b & ef, s: Cursor[a]): Cursor[b] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })

    ///
    /// Returns the result of applying `f` to every element in Cursor `s` and the accumulator `ac`.
    ///
    pub def mapAccum(f: (ac, a) -> (ac, b), ac: ac, s: Cursor[a]): Cursor[b] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })

    ///
    /// Pair each element of stream `s` with its index, the index starts at `k`.
    ///
    pub def index(k: Int32, s: Cursor[a]): Cursor[(Int32, a)] =
        mapAccum((ix, a) -> (ix + 1, (ix, a)), k, s)

    ///
    /// Pair each element of stream `s` with its index, the index starts at 0.
    ///
    pub def zeroIndex(s: Cursor[a]): Cursor[(Int32, a)] =
        mapAccum((ix,a) -> (ix+1, (ix, a)), 0, s)

    ///
    /// Returns a stream built from the elements of list `xs`.
    ///
    pub def fromList(xs: List[a]): Cursor[a] =
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
        Cursor({ generate = gen, finalize = _ -> () as & Impure })

    ///
    /// Returns a stream built from the elements of array `arr`.
    ///
    pub def fromArray(arr: Array[a]): Cursor[a] =
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
        Cursor({ generate = gen, finalize = _ -> () as & Impure })

    ///
    /// Add element `x` to the front of stream `s`.
    ///
    pub def cons(x: a, s: Cursor[a]) : Cursor[a] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })

    ///
    /// Deconstruct the stream `s`.
    ///
    /// If the stream is productive return `Some(a,rest)` where `a` is the first element
    /// and `rest` is the rest of the stream.
    ///
    /// If the stream is exhausted return `None`.
    ///
    pub def view(s: Cursor[a]): Option[(a, Cursor[a])] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        viewHelper(sf, finalize1)

    ///
    /// Helper function for `view`.
    ///
    def viewHelper(sf: Unit ~> Step[a], finalizer: Unit ~> Unit): Option[(a, Cursor[a])] & Impure =
        match sf() {
            case Done => { finalizer(); None }
            case Skip => viewHelper(sf, finalizer)
            case Yield(a) => {
                let tail =
                    Cursor({
                        generate = () ->
                            let next = () -> sf();
                            next as & Impure
                        ,
                        finalize = () -> finalizer()
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
    pub def head(s: Cursor[a]): Option[a] & Impure =
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
    pub def zip(s1: Cursor[a], s2: Cursor[b]): Cursor[(a,b)] = zipWith((a,b) -> (a,b), s1, s2)

    ///
    /// Returns a stream where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `s1` and `b` is the element at index `i` in `s2`.
    ///
    /// If either `s1` or `s2` becomes depleted, then no further elements are added to the resulting stream.
    ///
    pub def zipWith(f: (a,b) -> c, s1: Cursor[a], s2: Cursor[b]): Cursor[c] =
        let gen = () -> {
            let generate1 = getGenerator(s1);
            let generate2 = getGenerator(s2);
            let sf1 = generate1();
            let sf2 = generate2();
            let next = () -> zipWithHelper(f, sf1, sf2);
            next as & Impure
        };
        let final = () -> {
            let finalize1 = getFinalizer(s1);
            let finalize2 = getFinalizer(s2);
            finalize1();
            finalize2()
        };
        Cursor({ generate = gen, finalize = final })

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
    pub def fold(f: (b, a) -> b & e, ac: b, s: Cursor[a]): b & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = foldHelper(f, sf, ac);
        finalize1();
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
    pub def append(s1: Cursor[a], s2: Cursor[a]): Cursor[a] =
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
        let final = () -> { 
            let finalize1 = getFinalizer(s1);
            let finalize2 = getFinalizer(s2);
            finalize1();
            finalize2()
        };
        Cursor({ generate = gen, finalize = final })

    ///
    /// Returns the first `n` elements of stream `s`.
    ///
    /// Returns `s` if `n > length(s)`.
    /// Returns the empty stream if `n < 0`.
    ///
    pub def take(n: Int32, s: Cursor[a]): Cursor[a] =
        if (n <= 0)
            emptyOf(s)
        else
            takeHelper(n, s)

    ///
    /// Helper function for `take`.
    ///
    /// Precondition: n > 0 .
    ///
    def takeHelper(n: Int32, s: Cursor[a]): Cursor[a] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })


    ///
    /// Returns the longest prefix of `s` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhile(f: a -> Bool, s: Cursor[a]): Cursor[a] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })


    ///
    /// Returns `s` without the first `n` elements.
    ///
    /// Returns the empty stream if `n > length(xs)`.
    /// Returns `xs` if `n < 0`.
    ///
    pub def drop(n: Int32, s: Cursor[a]): Cursor[a] =
        if (n <= 0)
            s
        else
            dropHelper(n, s)

    ///
    /// Helper function for `drop`.
    ///
    /// Precondition: n >= 0 .
    ///
    def dropHelper(n: Int32, s: Cursor[a]): Cursor[a] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })

    ///
    /// Returns `s` without the longest prefix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhile(f: a -> Bool, s: Cursor[a]): Cursor[a] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })

    ///
    /// Returns a stream of every element in `s` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool, s: Cursor[a]): Cursor[a] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })

    ///
    /// Returns a stream of every element in `s` where `f` produces a value `Some(x)`.
    ///
    pub def choose(f: a -> Option[b] & e, s: Cursor[a]): Cursor[b] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })

    ///
    /// Returns `true` if streams `s1` and `s1` have the same elements, i.e. are equivalent.
    ///
    pub def sameElements[a: Eq](s1: Cursor[a], s2: Cursor[a]): Bool & Impure =
        let generate1 = getGenerator(s1);
        let generate2 = getGenerator(s2);
        let finalize1 = getFinalizer(s1);
        let finalize2 = getFinalizer(s2);
        let sf1 = generate1();
        let sf2 = generate2();
        let ans = sameElementsHelper(sf1, sf2);
        finalize1();
        finalize2();
        ans


    ///
    /// Helper function for `sameElements`.
    ///
    def sameElementsHelper[a: Eq](sf1: Unit ~> Step[a], sf2: Unit ~> Step[a]): Bool & Impure =
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
    def sameElementsHelperGoRight[a: Eq](x: a, sf1: Unit ~> Step[a], sf2: Unit ~> Step[a]) : Bool & Impure =
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
    def sameElementsHelperGoLeft[a: Eq](sf1: Unit ~> Step[a], y: a, sf2: Unit ~> Step[a]) : Bool & Impure =
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
    pub def slice(b: Int32, e: Int32, s: Cursor[a]): Cursor[a] =
        if (b >= e)
            emptyOf(s)
        else
            sliceHelper(b, e, s)

    ///
    /// Helper function for `slice`.
    ///
    /// Preconditions: b >= 0 && b < e
    ///
    pub def sliceHelper(b: Int32, e: Int32, s: Cursor[a]): Cursor[a] =
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
        Cursor({ generate = gen, finalize = getFinalizer(s) })


    ///
    /// Returns a stream with the element `a` repeated `n` times.
    ///
    /// Returns the empty stream if `n < 0`.
    ///
    pub def repeat(a: a, n: Int32): Cursor[a] =
        if (n <= 0)
            empty()
        else repeatHelper(a, n)

    ///
    /// Helper function for `repeat`.
    ///
    def repeatHelper(a: a, n: Int32): Cursor[a] =
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
        Cursor({ generate = gen, finalize = _ -> () as & Impure })

    ///
    /// Applies `f` to a start value `ac` and all `Ok` elements in `s` going from left to right.
    /// If an `Err` is encountered the stream closed and the error is returned.
    ///
    pub def foldResult(f: (b, a) -> Result[b, e] & ef, ac: b, s: Cursor[a]): Result[b, e] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = foldResultHelper(f, sf, ac);
        finalize1();
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
    pub def chooseToList(f: a -> Option[b] & e, s: Cursor[a]): List[b] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = chooseToListHelper(f, sf, ks -> ks as & Impure);
        finalize1();
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
    pub def collectToList(f: a -> List[b] & e, s: Cursor[a]): List[b] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = collectToListHelper(f, sf, ks -> ks as & Impure);
        finalize1();
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
    pub def traverseToListOption(f: a -> Option[b] & e, s: Cursor[a]): Option[List[b]] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = traverseToListOptionHelper(f, sf, _ -> None as & Impure,  ks -> Some(ks) as & Impure);
        finalize1();
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
    pub def traverseToListResult(f: a -> Result[b, e] & f, s: Cursor[a]): Result[List[b], e] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = traverseToListResultHelper(f, sf, e -> Err(e) as & Impure,  ks -> Ok(ks) as & Impure);
        finalize1();
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
    pub def foreachResult(f: a ~> Result[b, e], s: Cursor[a]): Result[Unit, e] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = foreachResultHelper(f, sf);
        finalize1();
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

    /// Don't provide `maximum`, `sum` etc. as they are terminal functions.

    ///
    /// Note this function is useful, but adding it means we are getting a very adhoc API...
    ///
    pub def resultFold(f: (b, a) -> b & ef, ac: b, s: Cursor[Result[a, e]]): Result[b, e] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = resultFoldHelper(f, sf, ac);
        finalize1();
        ans


    ///
    /// Helper function for `resultFold`.
    ///
    def resultFoldHelper(f: (b, a) -> b & ef, sf : Unit ~> Step[Result[a, e]], ac: b): Result[b, e] & Impure =
        match sf() {
            case Done => Ok(ac)
            case Skip => resultFoldHelper(f, sf, ac)
            case Yield(Ok(a)) => resultFoldHelper(f, sf, f(ac, a) as & Impure)
            case Yield(Err(e)) => Err(e)
        }


}
