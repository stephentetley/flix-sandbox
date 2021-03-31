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
///
/// WARNING - after testing, the outer generator / inner iterator style of the
/// current implementation doesn't seem appropriate for a cursor that is
/// usually expected to be used for e.g. streaming files.
/// Functions like `head` illustrate the problem - suppose you generated a
/// stream of time values with the code:
///
/// > let s1 = Cursor2.unfoldWithIter(_ -> Some(Time/LocalTime.now()))
///
/// > let h1 = Cursor2.head(s1)
/// > let h2 = Cursor2.head(s1)
/// > h1 != h2
///
/// h1 and h2 have different times - in simple terms s1 is regenerating
/// the stream each time head is called, whereas you might expect the stream to
/// generate the head once at construction time then suspend the stream until it
/// is "moved next".
///
/// With a pure generating function like `range` although the stream is regenerated
/// it looks the same because the generating function is pure and will generate an
/// identical stream.
///
/// > let s1 = Cursor2.range(1, 10)
///
/// > let h1 = Cursor2.head(s1)
/// > let h2 = Cursor2.head(s1)
/// > h1 == h2
///
/// As the use-case for for Cursor is IO where we will be opening file handles etc.
/// we really need a clearer and simpler idea of when we are creating resources so we
/// can be sure to finalize them.
///
pub opaque type Cursor2[e, a] =
    { generate: Unit ~> (Unit ~> Cursor2/Internal.Step2[e, a])
    , finalize: Unit ~> Unit
    }

namespace Cursor2/Internal {

    ///
    /// The internal type of iterator answers.
    /// `Skip` allows us to write `filter` and other functions that do not
    /// necessarily produce a value at each step.
    ///
    pub enum Step2[e, a] {
        case Done
        case Skip
        case Yield(a)
        case Failed(e)
    }

}


namespace Cursor2 {

    use Cursor2/Internal.Step2;
    use Cursor2/Internal.Step2.{Done, Skip, Yield, Failed};

    ///
    /// Returns the empty stream.
    ///
    pub def empty(): Cursor2[e, a] =
        let gen = () -> {
            /// Perform a redundant imperative action to get past the typechecker.
            let dummy = ref 0;
            let next = () -> {
                dummy := 1;
                Done
            };
            next
        };
        Cursor2({generate = gen, finalize = _ -> () as & Impure})

    ///
    /// Helper function to extract finalizer.
    ///
    pub def getFinalizer(s: Cursor2[e, a]): Unit ~> Unit =
        let Cursor2(body) = s;
        body.finalize

    ///
    /// Helper function to extract generator.
    ///
    pub def getGenerator(s: Cursor2[e, a]): Unit ~> (Unit ~> Step2[e, a]) =
        let Cursor2(body) = s;
        body.generate

    ///
    /// Truncate a Cursor but keep its finalizer.
    ///
    def emptyOf(s: Cursor2[e, a]): Cursor2[e, a] =
        let gen = () -> {
            let dummy = ref 0;
            let next = () -> {
                dummy := 1;
                Done
            };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})

    ///
    /// Returns a one element stream containing `x`.
    ///
    pub def return(x: a): Cursor2[e, a] =
        let gen = () -> {
            let called = ref false;
            let next = () -> {
                if (deref called)
                    Done
                else {
                    called := true;
                    Yield(x)
                }
            };
            next
        };
        Cursor2({generate = gen, finalize = _ -> () as & Impure})

    ///
    /// Returns a one element stream containing `x`.
    ///
    pub def throwError(err: e): Cursor2[e, a] =
        let gen = () -> {
            let called = ref false;
            let next = () -> {
                called := true;
                Failed(err)
            };
            next
        };
        Cursor2({generate = gen, finalize = _ -> () as & Impure})


    ///
    /// Returns the stream produced by applying the mapping function produced by Cursor2 `sf` to the elements
    /// produced by Cursor2 `s`.
    ///    
    pub def ap(sf: Cursor2[e, a -> b], s: Cursor2[e, a]): Cursor2[e, b] =
        zipWith((f,a) -> f(a), sf, s)


    pub def bind(s: Cursor2[e, a], k: a -> Cursor2[e, b]): Cursor2[e, b] =
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
                    case Skip => Skip
                    case Done => Done
                    case Failed(e)  => Failed(e)
                };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})

    pub def flatMap(k: a -> Cursor2[e, b], s: Cursor2[e, a]): Cursor2[e, b] = bind(s, k)

    ///
    /// Construct an Cursor2.
    ///
    /// The Cursor2 must provide three functions.
    ///
    /// * `hasNext` to test whether the iterator can still produce more values.
    ///
    /// * `next` to produce another value iff `hasNext` has indicated more values are available.
    ///
    /// * `finalize` to be called when the iterator has exhausted all its input. `finalize` should close
    ///    any open resources.
    ///
    type alias Cursor2Alg[e, a] = { hasNext: Unit ~> Bool, next: Unit ~> Result[a, e], finalize: Unit ~> Unit }

    /// Note: this implementation depletes imperative resources like file handles, this makes
    /// e.g. `length` a terminal operation, which it isn't for a "pure" stream.
    /// F# does not have this problem - imperative Seq's are replayable (is this due to reset?).
    ///
    /// Also F# is current / moveNext because that is what Java allows rather than hasNext / next
    /// which is the .net API.

    ///
    /// Returns a stream generated by an Cursor2.
    ///
    pub def makeCursor2(steps: Cursor2Alg[e, a]): Cursor2[e, a] =
        let hasNext = steps.hasNext;
        let iterNext = steps.next;
        let iterFinal = steps.finalize;
        let gen = () -> {
            let next = () -> {
                if (hasNext())
                    match iterNext() {
                        case Ok(a) => Yield(a)
                        case Err(e) => Failed(e)
                    }
                else
                    Done
            };
            next as & Impure
        };
        Cursor2({generate = gen, finalize = iterFinal})


    ///
    /// Returns a stream of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty Cursor2 if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Cursor2[e, Int32] =
        if (b >= e)
            empty()
        else
            rangeHelper(b, e)

    ///
    /// Helper function for `range`.
    ///
    /// Precodition: b < e
    ///
    def rangeHelper(b: Int32, e: Int32): Cursor2[e, Int32] =
        let gen = () -> {
            let rx = ref b;             /// Range show why we need effects in both the inner and outer thunks
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
        Cursor2({generate = gen, finalize = _ -> () as & Impure})

    ///
    /// Returns the stream `s` as a list.
    ///
    pub def toList(s: Cursor2[e, a]): Result[List[a], e] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = toListHelper(sf, e -> Err(e), xs -> Ok(xs));
        finalize1();
        ans

    ///
    /// Helper function for `toList
    ///
    def toListHelper(sf : Unit ~> Step2[e, a], 
                    fk: e -> Result[List[a], e], 
                    sk: List[a] -> Result[List[a], e]): Result[List[a], e] & Impure =
        match sf() {
            case Done => sk(Nil)
            case Skip => toListHelper(sf, fk, sk)
            case Yield(x) => toListHelper(sf, fk, xs -> sk(x :: xs))
            case Failed(e) => fk(e)
        }


    ///
    /// Applies `f` to every element of `s`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, s: Cursor2[e, a]): Result[Unit, e] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = foreachHelper(f, sf);
        finalize1();
        ans

    ///
    /// Helper function for `foreach`.
    ///
    def foreachHelper(f: a ~> Unit, sf : Unit ~> Step2[e, a]): Result[Unit, e] & Impure =
        match sf() {
            case Done => Ok()
            case Skip => foreachHelper(f, sf)
            case Yield(a) => {
                f(a);
                foreachHelper(f, sf)
            }
            case Failed(e) => Err(e)
        }


    ///
    /// Applies `f` to every element of `s` and its index.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreachWithIndex(f: (a, Int32) ~> Unit, s: Cursor2[e, a]): Result[Unit, e] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = foreachWithIndexHelper(f, 0, sf);
        finalize1();
        ans

    ///
    /// Helper function for `foreachWithIndex`.
    ///
    def foreachWithIndexHelper(f: (a, Int32) ~> Unit, ix: Int32, sf : Unit ~> Step2[e, a]): Result[Unit, e] & Impure =
        match sf() {
            case Done => Ok()
            case Skip => foreachWithIndexHelper(f, ix, sf)
            case Yield(a) => {
                f(a, ix);
                foreachWithIndexHelper(f, ix + 1, sf)
            }
            case Failed(e) => Err(e)
        }

    ///
    /// Build a stream by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(a,st1)` to signal a new element `a` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the stream.
    ///
    pub def unfold(f: b -> Option[(a,b)], st: b): Cursor2[e, a] =
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
        Cursor2({generate = gen, finalize = _ -> () as & Impure})

    ///
    /// Build a cursor by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a new element `a`.
    ///
    /// `next` should return `None` to signal the end of building the stream.
    ///
    pub def unfoldWithIter(f: () ~> Option[a]): Cursor2[e, a] =
        let gen = () -> {
            let next = () ->
                match f() {
                    case None => Done
                    case Some(x) => Yield(x)
                };
            next as & Impure
        };
        Cursor2({generate = gen, finalize = _ -> () as & Impure})

    ///
    /// Returns the result of applying `f` to every element in Cursor2 `s`.
    ///
    pub def map(f: a -> b & ef, s: Cursor2[e, a]): Cursor2[e, b] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () ->
                match sf() {
                    case Yield(a) => {
                        let a1 = f(a) as & Impure;
                        Yield(a1)
                    }
                    case Skip => Skip
                    case Done => Done
                    case Failed(e) => Failed(e)
                };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})

    ///
    /// Returns the result of applying `f` to every element in Cursor2 `s` and the accumulator `ac`.
    ///
    /// TODO this isn't strictly a `mapAccum` as state is not available at the end. 
    /// Reconsider the name or remove...
    /// 
    pub def mapAccum(f: (ac, a) -> (ac, b), ac: ac, s: Cursor2[e, a]): Cursor2[e, b] =
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
                    case Skip => Skip
                    case Done => Done
                    case Failed(e) => Failed(e)
                }
            };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})

    ///
    /// Pair each element of stream `s` with its index, the index starts at `k`.
    ///
    pub def index(k: Int32, s: Cursor2[e, a]): Cursor2[e, (Int32, a)] =
        mapAccum((ix, a) -> (ix + 1, (ix, a)), k, s)

    ///
    /// Pair each element of stream `s` with its index, the index starts at 0.
    ///
    pub def zeroIndex(s: Cursor2[e, a]): Cursor2[e, (Int32, a)] =
        mapAccum((ix,a) -> (ix+1, (ix, a)), 0, s)

    ///
    /// Returns a stream built from the elements of list `xs`.
    ///
    pub def fromList(xs: List[a]): Cursor2[e, a] =
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
        Cursor2({generate = gen, finalize = _ -> () as & Impure})

    ///
    /// Returns a stream built from the elements of array `arr`.
    ///
    pub def fromArray(arr: Array[a]): Cursor2[e, a] =
        let gen = () -> {
            let rx = ref 0;
            let next = () -> {
                let ix = deref rx;
                if (ix >= Array.length(arr))
                    Done
                else {
                    let p = Array.get(arr, ix);
                    rx := ix + 1;
                    Yield(p)
                }
            };
            next
        };
        Cursor2({generate = gen, finalize = _ -> () as & Impure})

    ///
    /// Add element `x` to the front of stream `s`.
    ///
    pub def cons(x: a, s: Cursor2[e, a]) : Cursor2[e, a] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let initial = ref true;
            let next = () -> {
                if (deref initial) {
                    initial := false;
                    Yield(x)
                } else 
                    sf()
            };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})


    ///
    /// Returns the head of `s`.
    ///
    /// If the stream is productive return `Some(a)` where `a` is the first element
    /// of `s`.
    ///

    /// If the stream is exhausted or in the failed state return `None`.
    ///
    pub def head(s: Cursor2[e, a]): Option[a] & Impure =
        let generate1 = getGenerator(s);
        let sf = generate1();
        headHelper(sf)

    ///
    /// Helper function for `head`.
    ///
    def headHelper(sf: Unit ~> Step2[e, a]): Option[a] & Impure =
        match sf() {
            case Done => None
            case Skip => headHelper(sf)
            case Yield(a) => Some(a)
            case Failed(_) => None
        }



    ///
    /// Returns a stream where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `s1` and `b` is the element at index `i` in `s2`.
    ///
    /// If either `s1` or `s2` becomes depleted, then no further elements are added to the resulting stream.
    ///
    pub def zip(s1: Cursor2[e, a], s2: Cursor2[e, b]): Cursor2[e, (a,b)] = zipWith((a,b) -> (a,b), s1, s2)

    ///
    /// Returns a stream where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `s1` and `b` is the element at index `i` in `s2`.
    ///
    /// If either `s1` or `s2` becomes depleted, then no further elements are added to the resulting stream.
    ///
    pub def zipWith(f: (a, b) -> c, s1: Cursor2[e, a], s2: Cursor2[e, b]): Cursor2[e, c] =
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
        Cursor2({generate = gen, finalize = final})

    ///
    /// Helper function for `zipWith`.
    ///
    /// Any case with `Done` is `Done`.
    ///
    def zipWithHelper(f: (a, b) -> c, sf1: Unit ~> Step2[e, a], sf2: Unit ~> Step2[e, b]) : Step2[e, c] & Impure =
        match (sf1(), sf2()) {
            case (Yield(a), Yield(b))   => Yield(f(a,b))
            case (Yield(a), Skip)       => zipWithHelperGoRight(f, a, sf2)
            case (Skip, Yield(b))       => zipWithHelperGoLeft(f, sf1, b)
            case (Skip, Skip)           => Skip
            case (Failed(e), _)         => Failed(e)
            case (_, Failed(e))         => Failed(e)
            case (_, _)                 => Done
        }

    ///
    /// Helper function for `zipWith`.
    ///
    /// Precondition: stream `s1` (left) has yielded the value `x` but `s2` (right) yielded a `Skip`.
    /// Go along s2 (right) to produce a value.
    ///
    def zipWithHelperGoRight(f: (a, b) -> c, x: a, sf: Unit ~> Step2[e, b]) : Step2[e, c] & Impure =
        match sf() {
            case Yield(y)   => Yield(f(x,y))
            case Done       => Done
            case Skip       => zipWithHelperGoRight(f, x, sf)
            case Failed(e)  => Failed(e)
        }

    ///
    /// Helper function for `zipWith`.
    ///
    /// Precondition: stream `s2` (right) has yielded the value `x` but `s1` (left) yielded a `Skip`.
    /// Go along s2 (left) to produce a value.
    ///
    def zipWithHelperGoLeft(f: (a, b) -> c, sf: Unit ~> Step2[e, a], y: b) : Step2[e, c] & Impure =
        match sf() {
            case Yield(x)   => Yield(f(x,y))
            case Done       => Done
            case Skip       => zipWithHelperGoLeft(f, sf, y)
            case Failed(e)  => Failed(e)
        }

    ///
    /// Applies `f` to a start value `ac` and all elements in `s` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def fold(f: (b, a) -> b & ef, ac: b, s: Cursor2[e, a]): Result[b, e] & Impure =
        let generate1 = getGenerator(s);
        let finalize1 = getFinalizer(s);
        let sf = generate1();
        let ans = foldHelper(f, sf, ac);
        finalize1();
        ans


    ///
    /// Helper function for `fold`.
    ///
    def foldHelper(f: (b, a) -> b & ef, sf : Unit ~> Step2[e, a], ac: b): Result[b, e] & Impure =
        match sf() {
            case Done => Ok(ac)
            case Skip => foldHelper(f, sf, ac)
            case Yield(a) => foldHelper(f, sf, f(ac, a) as & Impure)
            case Failed(e) => Err(e)
        }

    ///
    /// Returns `s2` appended to `s1`.
    ///
    pub def append(s1: Cursor2[e, a], s2: Cursor2[e, a]): Cursor2[e, a] =
        let gen = () -> {
            let generate1 = getGenerator(s1);
            let generate2 = getGenerator(s2);
            let sf1 = generate1();
            let sf2 = generate2();
            let cursor1Finished = ref false;
            let next = () ->
                if (deref cursor1Finished)
                    sf2()
                else
                    match sf1() {
                        case Skip => Skip
                        case Yield(a) => Yield(a)
                        case Done => {
                            cursor1Finished := true;
                            Skip
                        }
                        case Failed(e) => Failed(e)
                    };
            next
        };
        let final = () -> { 
            let finalize1 = getFinalizer(s1);
            let finalize2 = getFinalizer(s2);
            finalize1();
            finalize2()
        };
        Cursor2({generate = gen, finalize = final})

    ///
    /// Returns the first `n` elements of stream `s`.
    ///
    /// Returns `s` if `n > length(s)`.
    /// Returns the empty stream if `n < 0`.
    ///
    pub def take(n: Int32, s: Cursor2[e, a]): Cursor2[e, a] =
        if (n <= 0)
            emptyOf(s)
        else
            takeHelper(n, s)

    ///
    /// Helper function for `take`.
    ///
    /// Precondition: n > 0 .
    ///
    def takeHelper(n: Int32, s: Cursor2[e, a]): Cursor2[e, a] =
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
                        case Failed(e) => Failed(e)
                    }
                };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})


    ///
    /// Returns the longest prefix of `s` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhile(f: a -> Bool, s: Cursor2[e, a]): Cursor2[e, a] =
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
                    case Failed(e) => Failed(e)
                };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})


    ///
    /// Returns `s` without the first `n` elements.
    ///
    /// Returns the empty stream if `n > length(xs)`.
    /// Returns `xs` if `n < 0`.
    ///
    pub def drop(n: Int32, s: Cursor2[e, a]): Cursor2[e, a] =
        if (n <= 0)
            s
        else
            dropHelper(n, s)

    ///
    /// Helper function for `drop`.
    ///
    /// Precondition: n >= 0 .
    ///
    def dropHelper(n: Int32, s: Cursor2[e, a]): Cursor2[e, a] =
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
                case Failed(e) => Failed(e)
                };
            next
        };
        Cursor2({ generate = gen, finalize = getFinalizer(s) })

    ///
    /// Returns `s` without the longest prefix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhile(f: a -> Bool, s: Cursor2[e, a]): Cursor2[e, a] =
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
                    case Failed(e) => Failed(e)
                };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})

    ///
    /// Returns a stream of every element in `s` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool, s: Cursor2[e, a]): Cursor2[e, a] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let next = () ->
                match sf() {
                    case Done => Done
                    case Skip => Skip
                    case Yield(a) => if (f(a)) Yield(a) else Skip
                    case Failed(e) => Failed(e)                            
                };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})

    ///
    /// Returns a stream of every element in `s` where `f` produces a value `Some(x)`.
    ///
    pub def choose(f: a -> Option[b] & ef, s: Cursor2[e, a]): Cursor2[e, b] =
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
                    case Failed(e) => Failed(e)
                };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})

    ///
    /// Returns `true` if cursor `s1` and `s1` have the same elements, i.e. are equivalent.
    /// 
    /// Returns `false` if either (or both) cursors have failed.
    /// 
    /// Note this is a "terminal function" and will exhaust both Cursors.
    ///
    pub def sameElements[a, e](s1: Cursor2[e, a], s2: Cursor2[e, a]): Bool & Impure with Eq[a] =
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
    def sameElementsHelper[a, e](sf1: Unit ~> Step2[e, a], sf2: Unit ~> Step2[e, a]): Bool & Impure with Eq[a] =
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
            case (Failed(_), _)         => false
            case (_, Failed(_))         => false
        }

    ///
    /// Helper function for `sameElements`.
    ///
    /// Precondition: stream `sf1` (left) has yielded the value `x` but `sf2` (right) yielded a `Skip`.
    /// Go along sf2 (right) to produce a value, if same continue along both.
    ///
    def sameElementsHelperGoRight[a, e](x: a,
                                        sf1: Unit ~> Step2[e, a],
                                        sf2: Unit ~> Step2[e, a]) : Bool & Impure with Eq[a] =
        match sf2() {
            case Yield(y)   => if (x != y) false else sameElementsHelper(sf1, sf2)
            case Done       => false
            case Skip       => sameElementsHelperGoRight(x, sf1, sf2)
            case Failed(_)  => false
        }

    ///
    /// Helper function for `sameElements`.
    ///
    /// Precondition: stream `sf2` (right) has yielded the value `x` but `sf1` (left) yielded a `Skip`.
    /// Go along sf2 (left) to produce a value, if same continue along both.
    ///
    def sameElementsHelperGoLeft[a, e](sf1: Unit ~> Step2[e, a],
                                        y: a,
                                        sf2: Unit ~> Step2[e, a]) : Bool & Impure with Eq[a] =
        match sf1() {
            case Yield(x)   => if (x != y) false else sameElementsHelper(sf1, sf2)
            case Done       => true
            case Skip       => sameElementsHelperGoLeft(sf1, y, sf2)
            case Failed(_)  => false
        }

    ///
    /// Returns the substream of `xs` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// That is, an element at index `i` in `xs` is part of the returned sublist if and only if `i >= b` and `i < e`.
    /// Note: Indices that are out of bounds in `xs` are not considered (i.e. slice(b, e, xs) = slice(max(0,b), min(length(xs),e), xs)).
    ///
    pub def slice(b: Int32, e: Int32, s: Cursor2[e, a]): Cursor2[e, a] =
        if (b >= e)
            emptyOf(s)
        else
            sliceHelper(b, e, s)

    ///
    /// Helper function for `slice`.
    ///
    /// Preconditions: b >= 0 && b < e
    ///
    pub def sliceHelper(b: Int32, e: Int32, s: Cursor2[e, a]): Cursor2[e, a] =
        let gen = () -> {
            let generate1 = getGenerator(s);
            let sf = generate1();
            let rx = ref 0;
            let next = () ->
                match sf() {
                    case Done => Done
                    case Skip => Skip
                    case Yield(a) => {
                        let i = deref rx;
                        if (i < b or i >= e) {
                            rx := i + 1;
                            Skip
                        } else {
                            rx := i + 1;
                            Yield(a)
                        }
                    }
                    case Failed(err) => Failed(err)
                };
            next
        };
        Cursor2({generate = gen, finalize = getFinalizer(s)})


    ///
    /// Returns a stream with the element `a` repeated `n` times.
    ///
    /// Returns the empty stream if `n < 0`.
    ///
    pub def repeat(a: a, n: Int32): Cursor2[e, a] =
        if (n <= 0)
            empty()
        else repeatHelper(a, n)

    ///
    /// Helper function for `repeat`.
    ///
    def repeatHelper(a: a, n: Int32): Cursor2[e, a] =
        let gen = () -> {
            let rx = ref 0;
            let next = () -> {
                let i = deref rx;
                if (i < n) {
                    rx := i + 1;
                    Yield(a)
                } else 
                    Done            
            };
            next
        };
        Cursor2({generate = gen, finalize = _ -> () as & Impure})

    /// ///
    /// /// Applies `f` to a start value `ac` and all `Ok` elements in `s` going from left to right.
    /// /// If an `Err` is encountered the stream closed and the error is returned.
    /// ///
    /// pub def foldResult(f: (b, a) -> Result[b, e] & ef, ac: b, s: Cursor2[a]): Result[b, e] & Impure =
    ///     let generate1 = getGenerator(s);
    ///     let finalize1 = getFinalizer(s);
    ///     let sf = generate1();
    ///     let ans = foldResultHelper(f, sf, ac);
    ///     finalize1();
    ///     ans

    /// ///
    /// /// Helper function for `foldResult`.
    /// ///
    /// def foldResultHelper(f: (b, a) -> Result[b, err] & ef, sf: Unit ~> Step[a], ac: b): Result[b, err] & Impure =
    ///     match sf() {
    ///         case Done => Ok(ac)
    ///         case Skip => foldResultHelper(f, sf, ac)
    ///         case Yield(a) => {
    ///             let ans = f(ac, a) as & Impure;
    ///             match ans {
    ///                 case Err(err) => Err(err)
    ///                 case Ok(ac1) => foldResultHelper(f, sf, ac1)
    ///             }
    ///         }
    ///     }

    /// ///
    /// /// ...Collects all Somes, not short-circuiting...
    /// ///
    /// pub def chooseToList(f: a -> Option[b] & e, s: Cursor2[a]): List[b] & Impure =
    ///     let generate1 = getGenerator(s);
    ///     let finalize1 = getFinalizer(s);
    ///     let sf = generate1();
    ///     let ans = chooseToListHelper(f, sf, ks -> ks as & Impure);
    ///     finalize1();
    ///     ans

    /// ///
    /// /// Helper function for `chooseToList`.
    /// ///
    /// def chooseToListHelper(f: a -> Option[b] & f, sf: Unit ~> Step[a], k: List[b] ~> List[b]): List[b] & Impure =
    ///     match sf() {
    ///         case Done => k(Nil)
    ///         case Skip => chooseToListHelper(f, sf, k)
    ///         case Yield(a) => match f(a) {
    ///             case None => chooseToListHelper(f, sf, k)
    ///             case Some(x) => chooseToListHelper(f, sf, ks -> k(x :: ks))
    ///         }
    ///     }

    /// ///
    /// /// ...Multiple results at each step, not short-circuiting...
    /// ///
    /// pub def collectToList(f: a -> List[b] & e, s: Cursor2[a]): List[b] & Impure =
    ///     let generate1 = getGenerator(s);
    ///     let finalize1 = getFinalizer(s);
    ///     let sf = generate1();
    ///     let ans = collectToListHelper(f, sf, ks -> ks as & Impure);
    ///     finalize1();
    ///     ans

    /// ///
    /// /// Helper function for `collectToList`.
    /// ///
    /// def collectToListHelper(f: a -> List[b] & f, sf: Unit ~> Step[a], k: List[b] ~> List[b]): List[b] & Impure =
    ///     match sf() {
    ///         case Done => k(Nil)
    ///         case Skip => collectToListHelper(f, sf, k)
    ///         case Yield(a) => {
    ///             let xs = f(a);
    ///             collectToListHelper(f, sf, ks -> k(xs ::: ks))
    ///         }
    ///     }


    /// ///
    /// /// Short-circuiting, fails on first None...
    /// ///
    /// pub def traverseToListOption(f: a -> Option[b] & e, s: Cursor2[a]): Option[List[b]] & Impure =
    ///     let generate1 = getGenerator(s);
    ///     let finalize1 = getFinalizer(s);
    ///     let sf = generate1();
    ///     let ans = traverseToListOptionHelper(f, sf, _ -> None as & Impure,  ks -> Some(ks) as & Impure);
    ///     finalize1();
    ///     ans

    /// ///
    /// /// Helper function for `traverseToListOption`.
    /// ///
    /// def traverseToListOptionHelper(f: a -> Option[b] & f, sf: Unit ~> Step[a], fk: Unit ~> Option[List[b]], sk: List[b] ~> Option[List[b]]): Option[List[b]] & Impure =
    ///     match sf() {
    ///         case Done => sk(Nil)
    ///         case Skip => traverseToListOptionHelper(f, sf, fk, sk)
    ///         case Yield(a) => match f(a) {
    ///             case None => fk()
    ///             case Some(x) => traverseToListOptionHelper(f, sf, fk, ks -> sk(x :: ks))
    ///         }
    ///     }

    /// ///
    /// /// Short-circuiting, fails on first Err...
    /// ///
    /// pub def traverseToListResult(f: a -> Result[b, e] & f, s: Cursor2[a]): Result[List[b], e] & Impure =
    ///     let generate1 = getGenerator(s);
    ///     let finalize1 = getFinalizer(s);
    ///     let sf = generate1();
    ///     let ans = traverseToListResultHelper(f, sf, e -> Err(e) as & Impure,  ks -> Ok(ks) as & Impure);
    ///     finalize1();
    ///     ans

    /// ///
    /// /// Helper function for `traverseToListResult`.
    /// ///
    /// def traverseToListResultHelper(f: a -> Result[b, e] & f, sf: Unit ~> Step[a], fk: e ~> Result[List[b], e], sk: List[b] ~> Result[List[b], e]): Result[List[b], e] & Impure =
    ///     match sf() {
    ///         case Done => sk(Nil)
    ///         case Skip => traverseToListResultHelper(f, sf, fk, sk)
    ///         case Yield(a) => match f(a) {
    ///             case Err(err) => fk(err)
    ///             case Ok(x) => traverseToListResultHelper(f, sf, fk, ks -> sk(x :: ks))
    ///         }
    ///     }


    /// ///
    /// /// Applies `f` to every element of `s`.
    /// ///
    /// /// The function `f` must be impure.
    /// /// Fails on first error.
    /// ///
    /// pub def foreachResult(f: a ~> Result[b, e], s: Cursor2[a]): Result[Unit, e] & Impure =
    ///     let generate1 = getGenerator(s);
    ///     let finalize1 = getFinalizer(s);
    ///     let sf = generate1();
    ///     let ans = foreachResultHelper(f, sf);
    ///     finalize1();
    ///     ans

    /// ///
    /// /// Helper function for `foreachResult`.
    /// ///
    /// def foreachResultHelper(f: a ~> Result[b, e], sf : Unit ~> Step[a]): Result[Unit, e] & Impure =
    ///     match sf() {
    ///         case Done => Ok()
    ///         case Skip => foreachResultHelper(f, sf)
    ///         case Yield(a) => match f(a) {
    ///             case Ok(_) => foreachResultHelper(f, sf)
    ///             case Err(e) => Err(e)
    ///         }
    ///     }

    /// /// Don't provide `maximum`, `sum` etc. as they are terminal functions.

}
