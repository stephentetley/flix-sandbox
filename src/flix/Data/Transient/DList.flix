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
/// A DList
///
enum DList[a] {
    case DList(List[a] -> List[a])
}

namespace DList {

    pub def empty(): DList[a] = DList(identity)

    pub def singleton(x: a): DList[a] = DList(xs -> x :: xs)

    pub def append(a: DList[a], b: DList[a]): DList[a] = 
        let DList(f) = a; 
        let DList(g) = b;
        DList(f << g)

    pub def cons(x: a, a: DList[a]): DList[a] = 
        let DList(f) = a; 
        DList(xs -> x :: f(xs))

    pub def snoc(a: DList[a], x: a): DList[a] = 
        let DList(f) = a; 
        DList(f << (xs -> x :: xs))

    pub def toList(xs: DList[a]): List[a] =
        let DList(f) = xs;
        f(Nil)

}