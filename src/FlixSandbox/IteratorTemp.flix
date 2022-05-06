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

    pub def iterToResultList(iter: Iterator[Result[a, e]]): Result[List[a], e] & Impure = 
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

    @Lazy
    pub def dropL(n: Int32, iter: Iterator[a]): Iterator[a] = 
        if (n <= 0 )
            iter
        else {
            let Iterator(done, next) = iter;
            let done1 = () -> dropForward(n, done, next);
            Iterator(done1, next)
        }

    ///
    /// Fast forwards the given iterator n times 
    ///
    def dropForward(n: Int32, done: Done, next: Next[a]): Bool & Impure =
        def loop(i) = {
            if (done())
                true
            else 
                if (i <= 0) 
                    false
                else { 
                    let _ = next();
                    loop(i - 1)
                } 
        };
        loop(n)

    pub def cons(e: a, iter: Iterator[a]): Iterator[a] & Impure = 
        let Iterator(done, next) = iter;
        let front = ref Some(e);
        let done1 = () -> match deref front {
            case None    => done()
            case Some(_) => true
        };
        let next1 = () -> match deref front {
            case None    => next()
            case Some(a) => {
                front := None;
                a
            }
        };
        Iterator(done1, next1)

    @Lazy
    pub def dropWhileL(f: a -> Bool, iter: Iterator[a]): Iterator[a] & Impure = 
        let Iterator(done, next) = iter;
        match dropWhileForward(f, done, next) {
            case None => Iterator.empty()
            case Some(a) => cons(a, iter) 
        }
        

    ///
    /// Fast forwards the given iterator while applying f is true 
    ///
    def dropWhileForward(f: a -> Bool, done: Done, next: Next[a]): Option[a] & Impure =
        def loop() = {
            if (done())
                None
            else {
                let a = next();                
                if (f(a))
                    loop()
                else 
                    Some(a)
            }
        };
        loop()



    @Lazy
    pub def filterMapL(f: a -> Option[b], iter: Iterator[a]): Iterator[b] & Impure =
        let Iterator(done, next) = iter;

        // Allocate a reference to hold the next element.
        let cursor = ref None;

        let done1 = () -> match deref cursor {
            case None => match forwardOpt(f, done, next) {
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
    /// Fast forwards the given iterator until the next element that satisfies
    /// the given predicate `f`. Returns it (if it exists).
    ///
    def forwardOpt(f: a -> Option[b] & ef, done: Done, next: Next[a]): Option[b] & Impure =
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


}
