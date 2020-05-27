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

pub enum Ordering {
    case LT,
    case EQ,
    case GT
}

namespace Ordering {

    pub def fromInt32(x: Int32): Ordering = match x {
        case i if i < 0     => LT
        case i if i == 0    => EQ
        case _              => GT
    }

    pub def toInt32(x: Ordering): Int32 = match x {
        case LT => -1
        case EQ => 0
        case GT => 1
    }



}