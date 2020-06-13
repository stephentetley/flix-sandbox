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

use Data/Transient.DList;
use Data/Transient.DList.{DList};

namespace Data/Transient {

    ///
    /// A DList
    ///
    pub opaque type DList[a] = List[a] -> List[a]

}

namespace Data/Transient/DList {

    ///
    /// Helper
    ///
    def compose(f: b -> c, g: a -> b): a -> c = x -> f(g(x))

    pub def empty(): DList[a] = DList(identity)

    pub def singleton(x: a): DList[a] = DList(xs -> x :: xs)



    pub def append(a: DList[a], b: DList[a]): DList[a] = 
        let DList(f) = a; 
        let DList(g) = b;
        DList(xs -> f(g(xs)))

    pub def cons(x: a, a: DList[a]): DList[a] = 
        let DList(f) = a; 
        DList(xs -> x :: f(xs))

    pub def snoc(a: DList[a], x: a): DList[a] = 
        let DList(f) = a; 
        DList(compose(f, ys -> x :: ys))

    pub def toList(xs: DList[a]): List[a] =
        let DList(f) = xs;
        f(Nil)

    pub def fromList(xs: List[a]): DList[a] =
        DList(ys -> List.append(xs, ys))

    pub def apply(xs: DList[a], ys: List[a]): List[a] =
        let DList(f) = xs;
        f(ys)

    pub def concat(xss: List[DList[a]]): DList[a] =
        List.foldLeft(append, empty(), xss)

    pub def foldRight(f: (a,b) -> b, ac: b, xs: DList[a]): b =
        List.foldRight(f, ac, toList(xs))


}