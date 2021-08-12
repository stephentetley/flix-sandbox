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



namespace X/IteratorExtras {

    pub def sequenceToList(iter: Iterator[Result[a, err]]): Result[List[a], err] & Impure =
        sequenceToListHelper(iter, err -> Err(err), ks -> Ok(ks))

    def sequenceToListHelper(iter: Iterator[Result[a, err]],
                            fk: err -> Result[List[a], err],
                            sk: List[a] -> Result[List[a], err]): Result[List[a], err] & Impure =
        let Iterator(done, next) = iter;
        if (not done()) {
            match next() {
                case Err(e) => Err(e)
                case Ok(a) => sequenceToListHelper(iter, fk, ks -> sk(a :: ks))
            }
        }
        else sk(Nil)

    pub def foldLeftM(f: (b, a) -> Result[b, e] & f, s: b, iter: Iterator[Result[a, e]]): Result[b, e] & Impure =
        let Iterator(done, next) = iter;
        if (not done()) {
            match Result.flatMap(f(s), next()) {
                case Err(e) => Err(e)
                case Ok(s1) => foldLeftM(f, s1, iter)
            }
        }
        else Ok(s)

}
