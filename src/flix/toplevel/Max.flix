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


pub enum Max[a] with Eq, Order, ToString, Boxable { case Max(a) }

instance SemiGroup[Max[a]] with Order[a] {
    pub def combine(x: Max[a], y: Max[a]): Max[a] =
        let Max(x1) = x;
        let Max(y1) = y;
        Max(Order.max(x1, y1))
}

instance PartialOrder[Max[a]] with Order[a] {
    pub def lessEqual(x: Max[a], y: Max[a]): Bool =
        let Max(x1) = x;
        let Max(y1) = y;
        x1 <= y1
}

instance UpperBound[Max[a]] with UpperBound[a] {
    pub def maxValue(): Max[a] = Max(UpperBound.maxValue())
}

/// Not sure about this - should Max have a `Bottom` case?
instance LowerBound[Max[a]] with LowerBound[a] {
    pub def minValue(): Max[a] = Max(LowerBound.minValue())
}

instance MeetLattice[Max[a]] with Order[a] {
    /// /\
    pub def greatestLowerBound(x: Max[a], y: Max[a]): Max[a] =
        let Max(x1) = x;
        let Max(y1) = y;
        Max(Order.min(x1, y1))
}

instance JoinLattice[Max[a]] with Order[a] {
    /// \/
    pub def leastUpperBound(x: Max[a], y: Max[a]): Max[a] =
        let Max(x1) = x;
        let Max(y1) = y;
        Max(Order.max(x1, y1))
}
