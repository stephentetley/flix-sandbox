/*
 *  Copyright 2020 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// A type class for monoids, objects that support an associative binary
/// operation `combine` and neutral element `empty`.
///
pub class Monoid[a] {
    ///
    /// Returns a neutral element.
    ///
    pub def empty(): a

    ///
    /// Returns the result of __combining__ `x` and `y`.
    ///
    /// When Flix ha Superclasses this function should be moved into the
    /// class Semigroup.
    ///
    pub def combine(x: a, y: a): a
}

instance Monoid[Unit] {
    def empty(): Unit = ()
    def combine(_: Unit, _: Unit): Unit = ()
}

instance Monoid[String] {
    def empty(): String = ""
    def combine(x: String, y: String): String = x + y
}

instance Monoid[Option[a]] with [a : Monoid] {
    def empty(): Option[a] = None
    def combine(x: Option[a], y: Option[a]): Option[a] = match (x, y) {
        case (Some(x1), Some(y1)) => Some(Monoid.combine(x1, y1))
        case (a, None) => a
        case (None, b) => b
    }
}

instance Monoid[(a1, a2)] with [a1 : Monoid, a2 : Monoid] {
    def empty(): (a1, a2) = (Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2), y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2))
    }
}

instance Monoid[(a1, a2, a3)] with [a1 : Monoid, a2 : Monoid, a3: Monoid] {
    def empty(): (a1, a2, a3) = (Monoid.empty(), Monoid.empty(), Monoid.empty())
    def combine(x: (a1, a2, a3), y: (a1, a2, a3)): (a1, a2, a3) = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) => (Monoid.combine(x1, y1), Monoid.combine(x2, y2), Monoid.combine(x3, y3))
    }
}


instance Monoid[List[a]] {
    def empty(): List[a] = Nil
    def combine(x: List[a], y: List[a]): List[a] = List.append(x, y)
}

instance Monoid[Set[a]] {
    def empty(): Set[a] = Set.empty()
    def combine(x: Set[a], y: Set[a]): Set[a] = Set.union(x, y)
}
