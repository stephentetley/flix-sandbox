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
/// See PureScript: https://github.com/purescript/purescript-enums/blob/master/src/Data/Enum.purs
///
pub lawless class Enum[a] {
    ///
    /// ...
    ///
    pub def succ(x: a): Option[a]

    ///
    /// `prec`
    ///
    pub def prec(x: a): Option[a]
}

instance Enum[Unit] {
    pub def succ(_x: Unit): Option[Unit] = None
    pub def prec(_x: Unit): Option[Unit] = None
}

instance Enum[Bool] {
    pub def succ(x: Bool): Option[Bool] = if (x == false) Some(true) else None
    pub def prec(x: Bool): Option[Bool] = if (x == true) Some(false) else None
}

/// pub class BoundedEnum[a] with [a : Enum] {
    ///
    /// ...
    ///
    /// pub def toEnum(x: Int32): Option[a]

    ///
    /// `prec`
    ///
    /// pub def fromEnum(x: a): Int32
/// }
