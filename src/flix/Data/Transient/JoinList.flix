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
/// A JoinList (JoinLists have cheap append)
///
enum JoinList[t] {
    case Empty,
    case One(t),
    case Join(JoinList[t], JoinList[t])
}

namespace JoinList {

    pub def empty(): JoinList[a] = Empty

    pub def singleton(x: a): JoinList[a] = One(x)

    pub def append(a: JoinList[a], b: JoinList[a]): JoinList[a] = Join(a,b)

    pub def cons(x: a, a: JoinList[a]): JoinList[a] = Join(One(a),b)
    
    pub def snoc(a: JoinList[a], x: a): JoinList[a] = Join(a, One(b))

    pub def toList(a: JoinList[a]): List[a] = toListHelper(a, Nil, xs -> xs)

    def toListHelper(xs: JoinList[a], acc:List[a], cont: List[a] -> List[a]): List[a] =
        match xs {
            case Empty => cont(acc)
            case One(a) => cont(a :: acc)
            case Join(l, r) =>
                // Go Right first
                toListHelper(r, acc, acc1 ->
                    toListHelper(l, acc1, cont))
        }



}