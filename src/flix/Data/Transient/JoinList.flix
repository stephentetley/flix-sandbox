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

use Data/Transient.JoinList;
use Data/Transient.JoinList1.{Empty, One, Join};

namespace Data/Transient {

    ///
    /// A JoinList (JoinLists have cheap append)
    ///
    type alias JoinList[a] = JoinList1[a]

    enum JoinList1[t] {
        case Empty,
        case One(t),
        case Join(JoinList1[t], JoinList1[t])
    }

}

namespace Data/Transient/JoinList {

    pub def empty(): JoinList[a] = Empty

    pub def singleton(x: a): JoinList[a] = One(x)

    pub def append(a: JoinList[a], b: JoinList[a]): JoinList[a] = Join(a, b)

    pub def cons(x: a, a: JoinList[a]): JoinList[a] = Join(One(x), a)
    
    pub def snoc(a: JoinList[a], x: a): JoinList[a] = Join(a, One(x))

    pub def toList(a: JoinList[a]): List[a] = toListHelper(a, Nil, identity)

    ///
    /// Helper for `toList` in CPS.
    ///
    def toListHelper(xs: JoinList[a], ac:List[a], k: List[a] -> List[a]): List[a] =
        match xs {
            case Empty => k(ac)
            case One(a) => k(a :: ac)
            case Join(l, r) =>
                // Go Right first
                toListHelper(r, ac, ac1 ->
                    toListHelper(l, ac1, k))
        }

    pub def fromList(xs: List[a]): JoinList[a] =
        List.foldRight(cons, empty(), xs)

    pub def concat(xss: List[JoinList[a]]): JoinList[a] =
        List.foldLeft(append, empty(), xss)

    ///
    /// Right-associative fold of a JoinList.
    ///
    pub def foldRight(f: (a, b) -> b, ac: b, xs: JoinList[a]): b =
        foldRightHelper(f, ac, xs, identity)

    ///
    /// Helper for `foldRight` in CPS.
    ///
    def foldRightHelper(f: (a, b) -> b, ac: b, xs: JoinList[a], k: b -> b): b = match xs {
        case Empty => k(ac)
        case One(a) => k(f(a, ac))
        case Join(x,y) =>
            foldRightHelper(f, ac, y, ac1 -> foldRightHelper(f, ac1, x, ac2 -> k(ac2)))
    }


    ///
    /// Left-associative fold of a JoinList.
    ///
    pub def foldLeft(f: (b, a) -> b, ac: b, xs: JoinList[a]): b =
        foldLeftHelper(f, ac, xs, identity)

    ///
    /// Helper for `foldLeft` in CPS.
    ///
    def foldLeftHelper(f: (b, a) -> b, ac: b, xs: JoinList[a], k: b -> b): b = match xs {
        case Empty => k(ac)
        case One(a) => k(f(ac, a))
        case Join(x,y) =>
            foldLeftHelper(f, ac, x, ac1 -> foldLeftHelper(f, ac1, y, ac2 -> k(ac2)))
    }

    ///
    /// Returns a list with the element `a` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    pub def repeat(a: a, n: Int32): JoinList[a] = repeatHelper(a, n, Empty)

    def repeatHelper(a: a, n: Int32, ac: JoinList[a]): JoinList[a] =
        if (n <= 0)
            ac
        else {
            let ac1 = cons(a, ac);
            repeatHelper(a, n-1, ac1)
        }



}