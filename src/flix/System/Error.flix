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

use System.Error;
use System.Error.{ArrayIndexOutOfBoundsException,
        IllegalArgumentException,
        IllegalStateException,
        IndexOutOfBoundsException,
        InputMismatchException,
        IOException,
        NoSuchElementException,
        PatternSyntaxException,
        Generic};

namespace System {

    ///
    /// Represents an Error.
    ///
    pub enum Error {
        case ArrayIndexOutOfBoundsException,
        case IllegalArgumentException,
        case IllegalStateException,
        case IndexOutOfBoundsException,
        case InputMismatchException,
        case IOException,
        case NoSuchElementException,
        case PatternSyntaxException,
        case Generic(String)
    }

}

namespace System/Error {

    pub def toString(x: Error): String = match x {
        case ArrayIndexOutOfBoundsException     => "ArrayIndexOutOfBoundsException"
        case IllegalArgumentException           => "IllegalArgumentException"
        case IllegalStateException              => "IllegalStateException"
        case IndexOutOfBoundsException          => "IndexOutOfBoundsException"
        case InputMismatchException             => "InputMismatchException"
        case IOException                        => "IOException"
        case NoSuchElementException             => "NoSuchElementException"
        case PatternSyntaxException             => "PatternSyntaxException"
        case Generic(s)                         => s
    }

}