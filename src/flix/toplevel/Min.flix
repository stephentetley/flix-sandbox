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


pub enum Min[a] with Eq, Order, ToString, Boxable { case Min(a) }

instance SemiGroup[Min[a]] with Order[a] {
    pub def combine(x: Min[a], y: Min[a]): Min[a] =
        let Min(x1) = x;
        let Min(y1) = y;
        Min(Order.min(x1, y1))
}

instance PartialOrder[Min[a]] with Order[a] {
    pub def lessEqual(x: Min[a], y: Min[a]): Bool =
        let Min(x1) = x;
        let Min(y1) = y;
        x1 <= y1
}

instance LowerBound[Min[a]] with LowerBound[a] {
    pub def minValue(): Min[a] = Min(LowerBound.minValue())
}

instance MeetLattice[Min[a]] with Order[a] {
    /// /\
    pub def greatestLowerBound(x: Min[a], y: Min[a]): Min[a] =
        let Min(x1) = x;
        let Min(y1) = y;
        Min(Order.max(x1, y1))
}

instance JoinLattice[Min[a]] with Order[a] {
    /// \/
    pub def leastUpperBound(x: Min[a], y: Min[a]): Min[a] =
        let Min(x1) = x;
        let Min(y1) = y;
        Min(Order.min(x1, y1))
}
