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

namespace ListExtras {


    /// `Option.sequence` becomes `List.allSome`

    ///
    /// Returns `Some(v1 :: v2 :: ... :: vn)` if each of `xs_i` is `Some(v_i)`. Otherwise returns `None`.
    ///
    @Time(List.length(xs)) @Space(List.length(xs))
    pub def allSome(xs: List[Option[a]]): Option[List[a]] =
        allSomeHelper(xs, _ -> None, ys -> Some(ys))

    ///
    /// Helper function for `allSomeHelper`.
    ///
    def allSomeHelper(xs: List[Option[a]], fk: Unit -> Option[List[a]], sk: List[a] -> Option[List[a]]): Option[List[a]] = match xs {
        case Nil            => sk(Nil)
        case None :: _      => fk()
        case Some(y) :: rs  => allSomeHelper(rs, fk, ys -> sk(y :: ys))
    }

    ///
    /// Returns `Some(v1 :: v2 :: ... :: vn)` if each of `xs_i` is `Some(v_i)`. Otherwise returns `None`.
    ///
    @Time(List.length(xs)) @Space(List.length(xs))
    pub def catSomes(xs: List[Option[a]]): List[a] =
        catSomesHelper(xs, ks -> ks)

    ///
    /// Helper function for `allSomeHelper`.
    ///
    def catSomesHelper(xs: List[Option[a]], k: List[a] -> List[a]): List[a] = match xs {
        case Nil            => k(Nil)
        case None :: rs     => catSomesHelper(rs, ks -> k(ks))
        case Some(y) :: rs  => catSomesHelper(rs, ks -> k(y :: ks))
    }

    /// `Option.traverse` becomes `List.allSomeMap`
    /// Dubious name - if we want all then there isn't really any filtering being done.

    ///
    /// Returns `Some(v1 :: v2 :: ... v :: vn)` if each of `f(xs_i)` is `Some(v_i)`. Otherwise returns `None`.
    ///
    pub def allSomeMap(f: a -> Option[b] & ef, xs: List[a]): Option[List[b]] & ef =
        allSomeMapHelper(f, xs, _ -> None, ys -> Some(ys))

    ///
    /// Helper function for `allSomeMap`.
    ///
    def allSomeMapHelper(f: a -> Option[b] & f, xs: List[a], fk: Unit -> Option[List[b]], sk: List[b] -> Option[List[b]]): Option[List[b]] & f = match xs {
        case Nil            => sk(Nil)
        case x :: rs        => match f(x) {
            case None    => fk()
            case Some(y) => allSomeMapHelper(f, rs, fk, ys -> sk(y :: ys))
        }
    }

    /// `Option.traverseX` is an anomaly and the current version has a less than ideal type:
    ///
    /// > pub def traverseX(f: a -> Option[b] & ef, xs: List[a]): Option[Unit] & ef
    ///
    /// If we are evaluating `f` for it's effect as the doc says then it shouldn't bother
    /// producing a `b`. Instead this is a better type:
    ///
    /// > pub def traverseX(f: a -> Option[Unit] & ef, xs: List[a]): Option[Unit] & ef
    ///
    /// What we really have is a version of `List.foreach` that short-circuits on `None`.
    /// The salient point about the function is that it sort circuits.

    ///
    /// A version of `foreach` with "short-circuiting".
    ///
    /// `foreachSome` attempts to apply `f` to every element of `xs`.
    /// If `f` is successful, it will perform its effect and return `Some(())` to
    /// indicate the traversal is to continue. If `f` is unsuccessful it returns
    /// `None` to indicating the traversal will be stopped.
    ///
    /// At the end of the traversal, `foreachSome` returns with `None` indicating
    /// the traversal was incomplete, or `Some(())` indicating the traversal was
    /// sucessful.
    ///
    pub def foreachSome(f: a -> Option[Unit] & ef, xs: List[a]): Option[Unit] & ef = match xs {
        case Nil            => Some()
        case x :: rs        => match f(x) {
            case None    => None
            case Some(_) => foreachSome(f, rs)
        }
    }


}