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

namespace Data {

    ///
    /// "Beautiful Folds" style folding
    ///
    pub enum FoldL[xa, a, b] { 
        case FoldL(xa -> a -> xa, xa, xa -> b)
    }

}

namespace Data/FoldL {
    
    use Data.FoldL;
    use Data.FoldL.{FoldL};


    pub def foldLeft(alg: FoldL[xa, a, b], t: f[a]): b with Foldable[f] = 
        let FoldL(step, start, postp) = alg;
        Foldable.foldLeft(step, start, t) |> postp

    pub def mconcat(): FoldL[a, a, a] with Monoid[a] = 
        FoldL(Monoid.combine, Monoid.empty(), identity)

    pub def andF(): FoldL[Bool, Bool, Bool] = 
        FoldL((x, y) -> x and y, true, identity)

    pub def orF(): FoldL[Bool, Bool, Bool] = 
        FoldL((x, y) -> x or y, false, identity)

}
