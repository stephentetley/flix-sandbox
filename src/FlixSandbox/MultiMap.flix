/*
 * Copyright 2022 Stephen Tetley
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
/// The MultiMap type.
///
pub enum MultiMap[k, v] {
    case MultiMap(Map[k, Nec[v]])
}

namespace MultiMap {

    ///
    /// Returns the empty map.
    ///
    pub def empty(): MultiMap[k, v] = MultiMap(Map.empty())

    ///
    /// Returns the singleton multimap where key `k` is mapped to value `v`.
    ///
    pub def singleton(k: k, v: v): MultiMap[k, v] with Order[k] = MultiMap(Map.singleton(k, Nec.singleton(v)))

    ///
    /// Returns `true` if and only if `m` is the empty multimap.
    ///
    pub def isEmpty(m: MultiMap[k, v]): Bool =
        let MultiMap(m1) = m;
        Map.isEmpty(m1)

    ///
    /// Returns `Some(vs)` if `k => vs` is in `m`.
    ///
    /// Otherwise returns `Nil`.
    ///
    pub def get(k: k, m: MultiMap[k, v]): List[v] with Order[k] =
        let MultiMap(t) = m;
        Map.get(k, t) |> Option.map(Nec.toList) |> Option.getWithDefault(Nil)

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    pub def memberOf(k: k, m: MultiMap[k, v]): Bool with Order[k] =
        let MultiMap(m1) = m;
        Map.memberOf(k, m1)

    ///
    /// Returns the keys of `m`.
    ///
    pub def keysOf(m: MultiMap[k, v]): Set[k] with Order[k] =
        let MultiMap(m1) = m;
        Map.keysOf(m1)

    ///
    /// Updates `m` with `k => v`.
    ///
    pub def insert(k: k, v: v, m: MultiMap[k, v]): MultiMap[k, v] with Order[k] =
        let MultiMap(m1) = m;
        MultiMap(Map.insertWith((v1, v2) -> Nec.append(v1, v2), k, Nec.singleton(v), m1))



    ///
    /// Updates `m` with `k => map(f, vs)` if `k => vs` is in `m`.
    ///
    /// Otherwise, returns `m`.
    ///
    pub def adjust(f: v -> v & ef, k: k, m: MultiMap[k, v]): MultiMap[k, v] & ef with Order[k] =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => map(f(k), vs)` if `k => vs` is in `m`. Otherwise, returns `m`.
    ///
    pub def adjustWithKey(f: (k, v) -> v & ef, k: k, m: MultiMap[k, v]): MultiMap[k, v] & ef with Order[k] =
        updateWithKey((k1, v) -> Some(f(k1, v)), k, m)


    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    pub def update(f: v -> Option[v] & ef, k: k, m: MultiMap[k, v]): MultiMap[k, v] & ef with Order[k] =
        updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    pub def updateWithKey(f: (k, v) -> Option[v] & ef, k: k, m: MultiMap[k, v]): MultiMap[k, v] & ef with Order[k] =
        let MultiMap(m1) = m;
        let f1 = (k1, vs) -> Nec.filterMap(f(k1), vs) |> List.toNec;
        MultiMap(Map.updateWithKey(f1, k, m1))

    ///
    /// Removes the mapping `k` from the map `m`.
    ///
    pub def remove(k: k, m: MultiMap[k, v]): MultiMap[k, v] with Order[k] =
        let MultiMap(m1) = m;
        MultiMap(Map.remove(k, m1))

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: (k, v) -> Bool, m: MultiMap[k, v]): Option[(k, v)] = findLeft(f, m)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: (k, v) -> Bool, m: MultiMap[k, v]): Option[(k, v)] =
        let MultiMap(m1) = m;
        let f1 = (k, vs: Nec[v]) -> Nec.exists(f(k), vs);
        let f2 = (k, vs: Nec[v]) -> Nec.findLeft(f(k), vs) |> Option.map(v1 -> (k, v1));
        match Map.findLeft(f1, m1) {
            case None          => None
            case Some((k, vs)) => f2(k, vs)
        }

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: (k, v) -> Bool, m: MultiMap[k, v]): Option[(k, v)] =
        let MultiMap(m1) = m;
        let f1 = (k, vs: Nec[v]) -> Nec.exists(f(k), vs);
        let f2 = (k, vs: Nec[v]) -> Nec.findRight(f(k), vs) |> Option.map(v1 -> (k, v1));
        match Map.findRight(f1, m1) {
            case None          => None
            case Some((k, vs)) => f2(k, vs)
        }
/*
    ///
    /// Returns a map of all mappings `k => v` in `m` where `v` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: v -> Bool, m: MultiMap[k, v]): MultiMap[k, v] with Order[k] = filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k => v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filterWithKey(f: (k, v) -> Bool, m: MultiMap[k, v]): MultiMap[k, v] with Order[k] =
        foldLeftWithKey((acc, k, v) -> if (f(k, v)) insert(k, v, acc) else acc, empty(), m)
*/

    ///
    /// Returns a map with mappings `k => f(v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def map(f: v1 -> v2 & ef, m: MultiMap[k, v1]): MultiMap[k, v2] & ef = mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k => f(k, v)` for every `k => v` in `m`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    pub def mapWithKey(f: (k, v1) -> v2 & ef, m: MultiMap[k, v1]): MultiMap[k, v2] & ef =
        let MultiMap(m1) = m;
        let f1 = (k, vs) -> Nec.map(f(k), vs);
        MultiMap(Map.mapWithKey(f1, m1))


    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey(f: (b, k, v) -> b & ef, s: b, m: MultiMap[k, v]): b & ef = foldLeftWithKey(f, s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, v) -> b & ef, s: b, m: MultiMap[k, v]): b & ef =
        foldLeftWithKey((acc, _, v) -> f(acc, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, k1, v1), k2, v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (b, k, v) -> b & ef, s: b, m: MultiMap[k, v]): b & ef =
        let MultiMap(m1) = m;
        let f1 = (acc, k, vs) -> Nec.foldLeft((acc1, v1) -> f(acc1, k, v1), acc, vs);
        Map.foldLeftWithKey(f1, s, m1)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    pub def foldRight(f: (v, b) -> b & ef, s: b, m: MultiMap[k, v]): b & ef =
        foldRightWithKey((_, v, acc) -> f(v, acc), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b & ef, s: b, m: MultiMap[k, v]): b & ef =
        let MultiMap(m1) = m;
        let f1 = (k, vs, acc) -> Nec.foldRight((v1, acc1) -> f(k, v1, acc1), acc, vs);
        Map.foldRightWithKey(f1, s, m1)

    ///
    /// Applies `f` to a start value `z` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, z)))`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (v, Unit -> b & ef) -> b & ef, z: b, m: MultiMap[k, v]): b & ef =
        foldRightWithKeyCont((_, v, c) -> f(v, c), z, m)

    ///
    /// Applies `f` to a start value `z` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, z)))`.
    /// A `foldRightWithKeyCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithKeyCont(f: (k, v, Unit -> b & ef) -> b & ef, z: b, m: MultiMap[k, v]): b & ef =
        let MultiMap(m1) = m;
        Map.foldRightWithKeyCont((k, vs, b1) -> Nec.foldRightWithCont((v1, fac) -> f(k, v1, fac), b1(), vs), z, m1)

        /// ??? We call the outer continuation b1 - is this correct?

}
