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


namespace FlixSandbox/IteratorTemp {



    /// ///
    /// /// Cons element `e` at the front of iterator `iter`.
    /// ///
    /// /// Does *not* consume any elements from the iterator.
    /// ///
    /// /// The original iterator `iter` should not be reused.
    /// ///
    /// def cons(e: a, iter: Iterator[a, r]): Iterator[a, r] \ Write(r) =
    ///     let Iterator(done, next) = iter;
    ///     let front = ref Some(e) @ Scoped.regionOf(iter);
    ///     let done1 = () -> match deref front {
    ///         case None    => done()
    ///         case Some(_) => false
    ///     };
    ///     let next1 = () -> match deref front {
    ///         case None    => next()
    ///         case Some(a) => {
    ///             front := None;
    ///             a
    ///         }
    ///     };
    ///     Iterator(done1, next1)



    ///
    /// Returns the contents of `iter` as a list.
    ///
    /// Consumes the entire iterator.
    ///
    /// If any stage of the iterator is `Err(e)` then `Err(e)` is returned (first fail).
    /// If the iterator has no errors then the result is wrapped with `Ok`.
    ///
    pub def toResultList(iter: Iterator[Result[a, e], r]): Result[List[a], e] \ Read(r) =
        let Iterator(done, next) = iter;
        def loop(fk, sk) = {
            if (done())
                sk(Nil)
            else
                match next() {
                    case Ok(a)  => loop(fk, ks -> sk(a :: ks))
                    case Err(e) => fk(e)
                }
        };
        loop(e -> Err(e), ks -> Ok(ks))

}
