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


instance PartialOrder[Bool] {
    pub def lessEqual(x: Bool, y: Bool): Bool = x <= y
}

instance MeetLattice[Bool] {
    /// /\
    pub def greatestLowerBound(x: Bool, y: Bool): Bool = x and y
}

instance JoinLattice[Bool] {
    /// \/
    pub def leastUpperBound(x: Bool, y: Bool): Bool = x or y
}

instance LowerBound[Set[a]] with Order[a] {
    pub def minValue(): Set[a] = Set.empty()
}

instance PartialOrder[Set[a]] with Order[a] {
    pub def lessEqual(x: Set[a], y: Set[a]): Bool with Order[a] = x <= y
}


instance JoinLattice[Set[a]] with Order[a] {
    /// \/
    pub def leastUpperBound(x: Set[a], y: Set[a]): Set[a] with Order[a] = Set.union(x, y)
}

instance MeetLattice[Set[a]] with Order[a] {
    /// /\
    pub def greatestLowerBound(x: Set[a], y: Set[a]): Set[a] with Order[a] = Set.intersection(x, y)
}
