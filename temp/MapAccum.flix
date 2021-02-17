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

namespace Validation {

    pub def mapAccum(f: (s, t1) -> (s, t2) & ef, start: s, x: Validation[t1, e]): (s, Validation[t2, e]) & ef = match x {
        case Success(a) => {
            let (s1, b) = f(start, a);
            (s1, Success(b))
        }
        case Failure(es) => (start, Failure(es)) as & ef
    }

}


namespace Option {

    pub def mapAccum(f: (s, a) -> (s, b) & ef, start: s, x: Option[a]): (s, Option[b]) & ef = match x {
        case Some(a) => {
            let (s1, b) = f(start, a);
            (s1, Some(b))
        }
        case None => (start, None) as & ef
    }

}

namespace Result {

    pub def mapAccum(f: (s, a) -> (s, b) & ef, start: s, x: Result[a, e]): (s, Result[b, e]) & ef = match x {
        case Ok(a) => {
            let (s1, b) = f(start, a);
            (s1, Ok(b))
        }
        case Err(e) => (start, Err(e)) as & ef
    }

}

namespace Array {

    pub def mapAccumLeft(f: (s, a) -> (s, b) & ef, start: s, a: Array[a]): (s, Array[b]) & Impure =
        let len = length(a);
        if (len == 0)
            (start, [])
        else {
            let (s1, x1) = f(start, a[0]) as & Impure;
            let b = new(x1, len);
            let s2 = mapAccumLeftHelper(f, s1, a, b, len, 1);
            (s2, b)
        }


    ///
    /// Helper function for `mapAccumLeft`.
    ///
    def mapAccumLeftHelper(f: (s, a) -> (s, b) & ef, s1: s, a: Array[a], b: Array[b], len: Int32, i: Int32): s & Impure =
        if (i >= len)
            s1
        else {
            let (s2, x1) = f(s1, a[i]) as & Impure;
            b[i] = x1;
            mapAccumLeftHelper(f, s2, a, b, len, i+1)
        }

    pub def mapAccumRight(f: (s, a) -> (s, b) & ef, start: s, a: Array[a]): (s, Array[b]) & Impure =
        let len = length(a);
        if (len == 0)
            (start, [])
        else {
            let (s1, x1) = f(start, a[len - 1]) as & Impure;
            let b = new(x1, len);
            let s2 = mapAccumRightHelper(f, s1, a, b, len - 2);
            (s2, b)
        }


    ///
    /// Helper function for `mapAccumRight`.
    ///
    def mapAccumRightHelper(f: (s, a) -> (s, b) & ef, s1: s, a: Array[a], b: Array[b], i: Int32): s & Impure =
        if (i < 0 )
            s1
        else {
            let (s2, x1) = f(s1, a[i]) as & Impure;
            b[i] = x1;
            mapAccumRightHelper(f, s2, a, b, i - 1)
        }

}

namespace Nel {

    pub def mapAccumLeft(f: (s, a) -> (s, b) & ef, start: s, xs: Nel[a]): (s, Nel[b]) & ef = match xs {
        case Nel(h, l) => {
            let (s1, h1) = f(start, h);
            let (s2, l1) = List.mapAccumLeft(f, s1, l);
            (s2, Nel(h1, l1))
        }
    }

    pub def mapAccumRight(f: (s, a) -> (s, b) & ef, start: s, xs: Nel[a]): (s, Nel[b]) & ef = match xs {
        case Nel(h, l) => {
            let (s1, l1) = List.mapAccumRight(f, start, l);
            let (s2, h1) = f(s1, h);
            (s2, Nel(h1, l1))
        }
    }

}

namespace List {

    pub def mapAccumLeft(f: (s, a) -> (s, b) & ef, start: s, xs: List[a]): (s, List[b]) & ef =
        mapAccumLeftHelper(f, start, xs, (s1, ks) -> (s1, ks))

    ///
    /// Helper function for `mapAccumLeft`.
    ///
    def mapAccumLeftHelper(f: (s, a) -> (s, b) & ef, s1: s, xs: List[a], k: (s, List[b]) -> (s, List[b])): (s, List[b]) & ef =
        match xs {
            case Nil => k(s1, Nil)
            case x :: rs => {
                let (s2, x1) = f(s1, x);
                mapAccumLeftHelper(f, s2, rs, (s3, ks) -> k(s3, x1 :: ks))
            }
        }

    pub def mapAccumRight(f: (s, a) -> (s, b) & ef, start: s, xs: List[a]): (s, List[b]) & ef =
        mapAccumRightHelper(f, start, xs, (s1, ks) -> (s1, ks) as & ef)

    ///
    /// Helper function for `mapAccumRight`.
    ///
    def mapAccumRightHelper(f: (s, a) -> (s, b) & ef, s1: s, xs: List[a], k: (s, List[b]) -> (s, List[b]) & ef): (s, List[b]) & ef =
        match xs {
            case Nil => k(s1, Nil)
            case x :: rs => mapAccumRightHelper(f, s1, rs, (s2, ks) -> {let (s3, x1) = f(s2, x); k(s3, x1 :: ks)})
        }

}