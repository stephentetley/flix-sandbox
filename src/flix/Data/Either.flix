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

// Note - not in the Data namespace.

enum Either[a, b] {
    case Left(a),
    case Right(b)
}

namespace Either {


    ///
    /// Returns `true` iff `x` is `Left(_)`.
    ///
    pub def isLeft(x: Either[a, b]): Bool = match x {
        case Left(_) => true
        case Right(_) => false
    }

    ///
    /// Returns `true` iff `r` is `Right(_)`.
    ///
    pub def isRight(x: Either[a, b]): Bool = match x {
        case Left(_) => false
        case Right(_) => true
    }


    pub def either(f: a -> b, g: b -> c, x: Either[a, b]): c = match x {
        case Left(a) => f(a)
        case Right(b) => g(b)
    }

    pub def mapLeft(f: a -> c, x: Either[a, b]): Either[c, b] = match x {
        case Left(a) => Left(f(a))
        case Right(b) => Right(b)
    }

    pub def mapRight(g: b -> c, x: Either[a ,b]): Either[a, c] = match x {
        case Left(a) => Left(a)
        case Right(b) => Right(g(b))
    }
}