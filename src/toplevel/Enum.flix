/*
 *  Copyright 2021 Stephen Tetley
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
pub class Enum[a] with Order[a] {

    ///
    /// `succ`
    ///
    pub def succ(x: a): Option[a]

    ///
    /// `pred`
    ///
    pub def pred(x: a): Option[a]

    ///
    /// Retraction law
    ///
    law succRetractsPred: forall(x: a) with Order[a] . x |> (Enum.pred `Monad.kleisliLeft` Enum.succ `Monad.kleisliLeft` Enum.pred) == Enum.pred(x)

    ///
    /// Retraction law
    ///
    law predRetractsSucc: forall(x: a) with Order[a] . x |> (Enum.succ `Monad.kleisliLeft` Enum.pred `Monad.kleisliLeft` Enum.succ) == Enum.succ(x)

}

instance Enum[Unit] {
    pub def succ(_x: Unit): Option[Unit] = None
    pub def pred(_x: Unit): Option[Unit] = None
}

instance Enum[Bool] {
    pub def succ(x: Bool): Option[Bool] = if (x == false) Some(true) else None
    pub def pred(x: Bool): Option[Bool] = if (x == true) Some(false) else None
}


