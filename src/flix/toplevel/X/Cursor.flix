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

/// DEPRECATED - stdlib now has Iterator.

namespace X {

    pub opaque type Cursor[e, a] = X/Cursor.CursorImpl[e, a]

    type alias CursorAlg[e, a] = { hasNext :: Unit ~> Bool, next :: Unit ~> Result[a, e], finalize :: Unit ~> Unit }
}

namespace X/Cursor {

    use X.Cursor;
    use X.CursorAlg;
    use X.Cursor.{Cursor};

    type alias CursorImpl[e, a] = { hasNext :: Unit ~> Bool, next :: Unit ~> Step[e, a], finalize :: Unit ~> Unit }

    enum Step[e, a] {
        case Ans(a)
        case Skip
        case Failed(e)
    }

    pub def makeCursor(steps: CursorAlg[e, a]): Cursor[e, a] =
        let next = steps.next;
        let nextF = () -> match next() {
            case Ok(a) => Ans(a)
            case Err(a) => Failed(a)
        };
        Cursor({hasNext = steps.hasNext, next = nextF, finalize = steps.finalize})

    pub def guardThunk(thunk: Unit ~> a, err: e): Unit ~> Result[a, e] = match () ->
        try {
            Ok(thunk())
        } catch {
            case _: ##java.lang.Exception => Err(err)
        }


    pub def failure(err: e): Cursor[e, a] =
        Cursor({ hasNext = () -> false as & Impure
               , next = () -> Failed(err) as & Impure
               , finalize = () -> () as & Impure})

    pub def fold!(f: (b, a) -> b & ef, ac: b, xs: Cursor[e, a]): Result[b, e] & Impure =
        let Cursor(xs1) = xs;
        foldHelper(f, ac, xs1)

    def foldHelper(f: (b, a) -> b & ef, ac: b, o: CursorImpl[e, a]): Result[b, e] & Impure =
        let hasNext = o.hasNext;
        match (hasNext()) {
            case true => {
                let next = o.next;
                match next() {
                    case Ans(x1) => {let ac1 = f(ac, x1) as & Impure; foldHelper(f, ac1, o)}
                    case Skip => foldHelper(f, ac, o)
                    case Failed(e) => {
                        let finalize = o.finalize;
                        let _ = finalize();
                        Err(e)
                    }
                }
            }
            case false => {
                let finalize = o.finalize;
                let _ = finalize();
                Ok(ac)
            }
        }

    pub def map(f: a -> b & ef, xs: Cursor[e, a]): Cursor[e, b] =
        let Cursor(xs1) = xs;
        let next1 = () -> {let nextF = xs1.next; let x1 = nextF(); mapStep(f, x1)};
        let hasNext1 = xs1.hasNext;
        let finalize1 = xs1.finalize;
        Cursor({next = next1, hasNext = hasNext1, finalize = finalize1})

    def mapStep(f: a -> b & ef, x: Step[e, a]): Step[e, b] & Impure = match x {
        case Ans(x1) => Ans(f(x1) as & Impure)
        case Skip => Skip
        case Failed(e) => Failed(e)
    } as & Impure

    pub def mapResult(f: a -> Result[b, e] & ef, xs: Cursor[e, a]): Cursor[e, b] =
        let Cursor(xs1) = xs;
        let next1 = () -> {let nextF = xs1.next; let x1 = nextF(); mapResultStep(f, x1)};
        let hasNext1 = xs1.hasNext;
        let finalize1 = xs1.finalize;
        Cursor({next = next1, hasNext = hasNext1, finalize = finalize1})

    def mapResultStep(f: a -> Result[b, e] & ef, x: Step[e, a]): Step[e, b] & Impure = match x {
        case Ans(x1) => match f(x1) {
            case Ok(x2) => Ans(x2)
            case Err(e) => Failed(e)
        }
        case Skip => Skip
        case Failed(e) => Failed(e)
    } as & Impure

    pub def filter(f: a -> Bool, xs: Cursor[e, a]): Cursor[e, a] =
        let Cursor(xs1) = xs;
        let next1 = () -> {let nextF = xs1.next; let x1 = nextF(); filterStep(f, x1)};
        let hasNext1 = xs1.hasNext;
        let finalize1 = xs1.finalize;
        Cursor({next = next1, hasNext = hasNext1, finalize = finalize1})

    def filterStep(f: a -> Bool, x: Step[e, a]): Step[e, a] = match x {
        case Ans(x1) => if (f(x1)) Ans(x1) else Skip
        case Skip => Skip
        case Failed(e) => Failed(e)
    }

    pub def filterMap(f: a -> Option[b] & ef, xs: Cursor[e, a]): Cursor[e, b] =
        let Cursor(xs1) = xs;
        let next1 = () -> {let nextF = xs1.next; let x1 = nextF(); filterMapStep(f, x1)};
        let hasNext1 = xs1.hasNext;
        let finalize1 = xs1.finalize;
        Cursor({next = next1, hasNext = hasNext1, finalize = finalize1})

    def filterMapStep(f: a -> Option[b] & ef, x: Step[e, a]): Step[e, b] & Impure = match x {
        case Ans(x1) => match f(x1) as & Impure {
            case Some(b1) => Ans(b1)
            case None => Skip
        }
        case Skip => Skip
        case Failed(e) => Failed(e)
    }


    pub def foreach!(f: a ~> Unit, xs: Cursor[e, a]): Result[Unit, e] & Impure =
        let Cursor(xs1) = xs;
        foreachHelper(f, xs1)

    def foreachHelper(f: a ~> Unit, o: CursorImpl[e, a]): Result[Unit, e] & Impure =
        let hasNext = o.hasNext;
        match (hasNext()) {
            case true => {
                let next = o.next;
                match next() {
                    case Ans(x1) => {let _ = f(x1); foreachHelper(f, o)}
                    case Skip => foreachHelper(f, o)
                    case Failed(e) => {
                        let finalize = o.finalize;
                        let _ = finalize();
                        Err(e)
                    }
                }
            }
            case false => {
                let finalize = o.finalize;
                finalize();
                Ok()
            }
        }

    ///
    /// This should only be called after a call to `hasNext` has indicated more data is available.
    ///
    pub def next!(xs: Cursor[e, a]): Result[a, e] & Impure =
        let Cursor(xs1) = xs;
        let next = xs1.next;
        match next() {
            case Ans(a) => Ok(a)
            case Skip => next!(xs)
            case Failed(e) => Err(e)
        }

    pub def hasNext(xs: Cursor[e, a]): Bool & Impure =
        let Cursor(xs1) = xs;
        let hasNext = xs1.hasNext;
        hasNext()

    pub def finalize!(xs: Cursor[e, a]): Unit & Impure =
        let Cursor(xs1) = xs;
        let finalize = xs1.finalize;
        finalize()

    pub def toList!(src: Cursor[e, a]): Result[List[a], e] & Impure =
        let Cursor(src1) = src;
        toListHelper(src1, e -> Err(e), ks -> Ok(ks))

    def toListHelper(o: CursorImpl[e, a],
                    fk: e -> Result[List[a], e],
                    sk: List[a] -> Result[List[a], e]): Result[List[a], e] & Impure =
        let hasNext = o.hasNext;
        match hasNext() {
            case true => {
                let next = o.next;
                match next() {
                    case Ans(x1) => toListHelper(o, fk, ks -> sk(x1 :: ks))
                    case Skip => toListHelper(o, fk, sk)
                    case Failed(e) => {
                        let finalize = o.finalize;
                        let _ = finalize();
                        fk(e)
                    }
                }
            }
            case false => {
                let finalize = o.finalize;
                let _ = finalize();
                sk(Nil)
            }
        }

    pub def toSet!(src: Cursor[e, a]): Result[Set[a], e] & Impure with Order[a] =
        fold!((s, x) -> Set.insert(x, s), Set#{}, src)

    pub def toMap!(src: Cursor[e, (a, b)]): Result[Map[a, b], e] & Impure with Order[a] =
        fold!((s, x) -> {let (k,v) = x; Map.insert(k, v, s)}, Map#{}, src)


    pub def foldResult!(f: (b, a) -> Result[b, e] & ef, ac: b, xs: Cursor[e, a]): Result[b, e] & Impure =
        let Cursor(xs1) = xs;
        foldResultHelper(f, ac, xs1)

    def foldResultHelper(f: (b, a) -> Result[b, e] & ef, ac: b, o: CursorImpl[e, a]): Result[b, e] & Impure =
        let hasNext = o.hasNext;
        match (hasNext()) {
            case true => {
                let next = o.next;
                match next() {
                    case Ans(x1) => match f(ac, x1) {
                        case Ok(ac1) => foldResultHelper(f, ac1, o)
                        case Err(e) => {
                            let finalize = o.finalize;
                            let _ = finalize();
                            Err(e)
                        }
                    }
                    case Skip => foldResultHelper(f, ac, o)
                    case Failed(e) => {
                        let finalize = o.finalize;
                        let _ = finalize();
                        Err(e)
                    }
                }
            }
            case false => {
                let finalize = o.finalize;
                let _ = finalize();
                Ok(ac)
            }
        }

    pub def foreachResult!(f: a ~> Result[b, e], xs: Cursor[e, a]): Result[Unit, e] & Impure =
        let Cursor(xs1) = xs;
        foreachResultHelper(f, xs1)

    def foreachResultHelper(f: a ~> Result[b, e], o: CursorImpl[e, a]): Result[Unit, e] & Impure =
        let hasNext = o.hasNext;
        match (hasNext()) {
            case true => {
                let next = o.next;
                match next() {
                    case Ans(x1) => match f(x1) {
                        case Ok(_) => foreachResultHelper(f, o)
                        case Err(e) => {
                            let finalize = o.finalize;
                            let _ = finalize();
                            Err(e)
                        }
                    }
                    case Skip => foreachResultHelper(f, o)
                    case Failed(e) => {
                        let finalize = o.finalize;
                        let _ = finalize();
                        Err(e)
                    }
                }
            }
            case false => {
                let finalize = o.finalize;
                finalize();
                Ok()
            }
        }

    pub def head!(src: Cursor[e, a]): Option[a] & Impure =
        if (hasNext(src)) {
            match next!(src) {
                case Ok(a) => Some(a)
                case Err(_) => None
            }
        } else None as & Impure


    pub def drop!(n: Int32, src: Cursor[e, a]): Result[Unit, e] & Impure =
        if (n <= 0)
            Ok() as & Impure
        else {
            head!(src);
            drop!(n - 1, src)
        }

    pub def take(n: Int32, src: Cursor[e, a]): Cursor[e, a] & Impure =
        let Cursor(src1) = src;
        let rx = ref 0;
        let hasNext0 = src1.hasNext;
        let hasNext1 = () -> {
            let ix = deref rx;
            if (ix >= n)
                false
            else {
                rx := ix + 1;
                hasNext0()
            }
        };
        Cursor({hasNext = hasNext1 | src1})

    pub def drop(n: Int32, src: Cursor[e, a]): Cursor[e, a] & Impure =
        let Cursor(src1) = src;
        let rx = ref 0;
        let next0 = src1.next;
        let next1 = () -> {
            match next0() {
                case Ans(a1) => {
                    let ix = deref rx;
                    rx := ix + 1;
                    if (ix < n) Skip else Ans(a1)
                }
                case Skip => Skip
                case Failed(e) => Failed(e)
            }
        };
        Cursor({next = next1 | src1})

}
