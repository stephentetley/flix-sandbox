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

namespace System/Internal/Utils {

    pub def unfoldList(f: s -> Option[(a, s)] & e, st: s): List[a] & e = unfoldListHelper(f, st, xs -> xs)

    def unfoldListHelper(f: s -> Option[(a, s)] & e, st: s, cont: List[a] -> List[a]): List[a] & e = match f(st) {
        case None => cont(Nil)
        case Some(a, st1) => unfoldListHelper(f, st1, xs -> cont(a :: xs))
    }

    pub def unfoldListWithIterator(next: () -> Option[a] & e): List[a] & e = unfoldListWithIteratorHelper(next, xs -> xs)

    def unfoldListWithIteratorHelper(next: () -> Option[a] & e, cont: List[a] -> List[a]): List[a] & e = match next() {
        case None => cont(Nil)
        case Some(a) => unfoldListWithIteratorHelper(next, xs -> cont(a :: xs))
    }

    pub def unfoldMap[s, e, v, k: Order](f: s -> Option[(k, v, s)] & e, st: s): Map[k,v] & e = unfoldMapHelper(f, st, m -> m)

    pub def unfoldMapHelper[s, e, v, k: Order](f: s -> Option[(k, v, s)] & e, st: s, cont: Map[k,v] -> Map[k,v]): Map[k,v] & e = match f(st) {
        case None => cont(Map.empty())
        case Some((k, v, ac1)) => unfoldMapHelper(f, ac1, m -> cont(Map.insert(k, v, m)))
    }

    pub def unfoldMapWithIterator[e, v, k: Order](next: () -> Option[(k, v)] & e): Map[k,v] & e =
        unfoldMapWithIteratorHelper(next, m -> m)

    pub def unfoldMapWithIteratorHelper[e, v, k: Order](next: () -> Option[(k, v)] & e, cont: Map[k,v] -> Map[k,v]): Map[k,v] & e =
        match next() {
            case None => cont(Map.empty())
            case Some((k, v)) => unfoldMapWithIteratorHelper(next, m -> cont(Map.insert(k, v, m)))
        }

}