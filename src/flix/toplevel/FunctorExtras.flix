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


namespace FunctorExtras {

    pub def ignore(ma: f[a]): f[Unit] with Functor[f] = ma `Functor.as` ()

    ///
    /// `<&>` is an infix synonym for `Functor.map`.
    ///
    pub def <&>(f: a -> b & ef, ma: f[a]): f[b] & ef with Functor[f] = Functor.map(f, ma)

    ///
    /// `<&` is an infix synonym for `Functor.as`.
    ///
    pub def <&(x: a, ma: f[b]): f[a] with Functor[f] = Functor.as(ma, x)

    ///
    /// `<&` is `&>` with its arguments flipped.
    ///
    pub def &>(ma: f[a], x: b): f[b] with Functor[f] = Functor.as(ma, x)

}
