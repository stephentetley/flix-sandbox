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

    ///
    /// Returns an iterator with every element of the iterator `iter` that
    /// produces `Some(_)` the from the function `f`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    @Lazy
    pub def filterMapL(f: a -> Option[b], iter: Iterator[a]): Iterator[b] & Impure =
        let Iterator(done, next) = iter;

        // Allocate a reference to hold the next element.
        let cursor = ref None;

        let done1 = () -> match deref cursor {
            case None => match filterMapForward(f, done, next) {
                case None    => true
                case Some(x) =>
                    // An element was found. Store it in the reference.
                    cursor := Some(x);
                    false
            }
            case Some(_) => false
        };

        let next1 = () -> match deref cursor {
            case None    => bug!("Empty iterator")
            case Some(x) =>
                // An element is in the reference. Use it.
                cursor := None;
                x
        };
        Iterator(done1, next1)

    ///
    /// Fast forwards the given iterator until the next element that produces
    /// a `Some` given function `f`. Returns it if it exists, or `None`.
    ///
    def filterMapForward(f: a -> Option[b] & ef, done: Done, next: Next[a]): Option[b] & Impure =
        def loop() = {
            if (done())
                None
            else
                let e = next();
                match f(e) {
                    case None => loop()
                    case Some(b) => Some(b)
                }
        };
        loop()

    ///
    /// Returns `iter` without the first `n` elements.
    ///
    /// Returns an empty iterator if `n > length(l)`.
    /// Returns `iter` if `n < 0`.
    ///
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    @Lazy
    pub def dropL(n: Int32, iter: Iterator[a]): Iterator[a] & Impure =
        if (n <= 0 )
            iter
        else {
            let Iterator(done, next) = iter;
            match dropForward(n, done, next) {
                case Some(iter1) => iter1
                case None        => Iterator.empty()
            }
        }

    ///
    /// Fast forwards the given iterator n times
    ///
    def dropForward(n: Int32, done: Done, next: Next[a]): Option[Iterator[a]] & Impure =
        let skipPos = ref n;
        def loop() = {
            let pos = deref skipPos;
            if (pos <= 0)
                Some(Iterator(done, next))
            else if (done())
                None
            else {
                let _ = next();
                skipPos := pos -1;
                loop()
            }
        };
        loop()


    ///
    /// Cons element `e` at the front of iterator `iter`.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    def cons(e: a, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;
        let front = ref Some(e);
        let done1 = () -> match deref front {
            case None    => done()
            case Some(_) => false
        };
        let next1 = () -> match deref front {
            case None    => next()
            case Some(a) => {
                front := None;
                a
            }
        };
        Iterator(done1, next1)

    ///
    /// Returns `iter` without the prefix of elements that satisfy
    /// the predicate `f` dropped.
    ///
    /// Does *not* consume any elements from the iterator.
    ///
    /// The original iterator `iter` should not be reused.
    ///
    @Lazy
    pub def dropWhileL(f: a -> Bool, iter: Iterator[a]): Iterator[a] & Impure =
        let Iterator(done, next) = iter;
        match dropWhileForward(f, done, next) {
            case None        => Iterator.empty()
            case Some(iter1) => iter1
        }

    ///
    /// Fast forwards the given iterator while applying f is true
    ///
    def dropWhileForward(f: a -> Bool, done: Done, next: Next[a]): Option[Iterator[a]] & Impure =
        def loop() = {
            if (done())
                None
            else {
                let a = next();
                if (f(a))
                    loop()
                else
                    Some(cons(a, Iterator(done, next)))
            }
        };
        loop()

    ///
    /// Returns the contents of `iter` as a list.
    ///
    /// Consumes the entire iterator.
    ///
    /// If any stage of the iterator is `Err(e)` then `Err(e)` is returned (first fail).
    /// If the iterator has no errors then the result is wrapped with `Ok`.
    ///
    pub def toResultList(iter: Iterator[Result[a, e]]): Result[List[a], e] & Impure =
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
