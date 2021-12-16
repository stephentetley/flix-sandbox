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

pub class Traversable[t : Type -> Type] with Functor[t] {

    pub def traverse(f: a -> m[b] & ef, t: t[a]): m[t[b]] & ef with Applicative[m]

    pub def sequence(t: t[m[a]]): m[t[a]] with Applicative[m] =
        Traversable.traverse(identity, t)

    ///
    /// Traversing with the identity function wrapped into an applicative preserves the container `t`.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] . Traversable.traverse(f, t) == Applicative.point(t)

    ///
    /// sequence identity.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] . Traversable.sequence(Functor.map(f, t)) == Applicative.point(t)

}


instance Traversable[List] {

    pub def traverse(f: a -> m[b] & ef, t: List[a]): m[List[b]] & ef with Applicative[m] = ListHelper.traverse(f, t)

    pub override def sequence(t: List[m[a]]): m[List[a]] with Applicative[m] = ListHelper.sequence(t)
}

instance Traversable[Nel] {
    /// CPS transformed...
    pub def traverse(f: a -> m[b] & ef, t: Nel[a]): m[Nel[b]] & ef with Applicative[m] = NelHelper.traverse(f, t)

    pub override def sequence(t: Nel[m[a]]): m[Nel[a]] with Applicative[m] = NelHelper.sequence(t)
}

instance Traversable[Chain] {
    /// CPS transformed...
    pub def traverse(f: a -> m[b] & ef, t: Chain[a]): m[Chain[b]] & ef with Applicative[m] = ChainHelper.traverse(f, t)

    pub override def sequence(t: Chain[m[a]]): m[Chain[a]] with Applicative[m] = ChainHelper.sequence(t)
}

instance Traversable[RedBlackTree.RedBlackTree[k]] {
    pub def traverse(f: a -> m[b] & ef, t: RedBlackTree.RedBlackTree[k, a]): m[RedBlackTree.RedBlackTree[k, b]] & ef with Applicative[m] = RedBlackTreeHelper.traverse(f, t)

    pub override def sequence(t: RedBlackTree.RedBlackTree[k, m[a]]): m[RedBlackTree.RedBlackTree[k, a]] with Applicative[m] = RedBlackTreeHelper.sequence(t)

}


instance Traversable[Map[k]] {
    pub def traverse(f: a -> m[b] & ef, t: Map[k, a]): m[Map[k, b]] & ef with Applicative[m] = MapHelper.traverse(f, t)

    pub override def sequence(t: Map[k, m[a]]): m[Map[k, a]] with Applicative[m] = MapHelper.sequence(t)

}

instance Traversable[Option] {
    pub def traverse(f: a -> m[b] & ef, t: Option[a]): m[Option[b]] & ef with Applicative[m] =
        use Applicative.{point};
        use Functor.{map};
        match t {
            case None => point(None)
            case Some(a) => f(a) |> map(Some)
        }

    pub override def sequence(t: Option[m[a]]): m[Option[a]] with Applicative[m] =
        use Applicative.{point};
        use Functor.{map};
        match t {
            case Some(ma) => map(Some, ma)
            case None => point(None)
        }

}

namespace Traversable {

    pub def for(t: t[a], f: a -> m[b] & ef): m[t[b]] & ef with Applicative[m], Traversable[t] =
        Traversable.traverse(f, t)

    enum Either[l, r] {
        case Left(l)
        case Right(r)
    }

    instance Functor[Either[e]] {
        pub def map(f: a -> b & ef, x: Either[e, a]): Either[e, b] & ef = match x {
            case Left(l) => Left(l) as & ef
            case Right(r) => Right(f(r))
        }
    }

    instance Applicative[Either[e]] {
        pub def point(x: a): Either[e, a] = Right(x)

        pub def ap(mf: Either[e, a -> b & ef], mx: Either[e, a]): Either[e, b] & ef = match mf {
            case Right(f) => match mx {
                case Right(x) => Right(f(x))
                case Left(a1) => Left(a1)
            }
            case Left(a) => Left(a)
        }
    }


    pub def sequenceOption(t: t[Option[a]]): Option[t[a]] with Traversable[t] = sequence(t)

    pub def traverseOption(f: a -> Option[b] & ef, t: t[a]): Option[t[b]] & ef with Traversable[t] =
        traverse(f, t)

    pub def sequenceResult(t: t[Result[a, e]]): Result[t[a], e] with Traversable[t] =
        let step = x -> match x {
            case Err(e) => Left(e)
            case Ok(a) => Right(a)
        };
        let final = x -> match x {
            case Left(e) => Err(e)
            case Right(a) => Ok(a)
        };
        traverse(step, t) |> final


    pub def mapAccumLeft(f: acc -> a -> (acc, b), start: acc, t: t[a]): (acc, t[b]) with Traversable[t] =
        runStateL(traverse(x1 -> StateL(flip(f, x1)), t), start)

    opaque type StateL[s, a] = s -> (s, a)

    instance Functor[StateL[s]] {
        pub def map(f: a -> b & ef, x: StateL[s, a]): StateL[s, b] & ef=
            let StateL(x1) = x;
            StateL(s ->
                let (s1, v1) = x1(s);
                (s1, f(v1) as & Pure)
            ) as & ef
    }

    instance Applicative[StateL[s]] {
        pub def point(x: a): StateL[s, a] = StateL(s -> (s, x))

        pub def ap(f: StateL[s, a -> b & ef], x: StateL[s, a]): StateL[s, b] & ef =
            let StateL(f1) = f;
            let StateL(x1) = x;
            StateL(s ->
                let (s1, f2) = f1(s);
                let (s2, v) = x1(s1);
                (s2, f2(v) as & Pure)
            ) as & ef
    }

    def runStateL(x: StateL[s, a], state: s): (s, a) =
        let StateL(x1) = x;
        x1(state)

    pub def mapAccumRight(f: acc -> a -> (acc, b), start: acc, t: t[a]): (acc, t[b]) with Traversable[t] =
        runStateR(traverse(x1 -> StateR(flip(f, x1)), t), start)

    opaque type StateR[s, a] = s -> (s, a)

    instance Functor[StateR[s]] {
        pub def map(f: a -> b & ef, x: StateR[s, a]): StateR[s, b] & ef =
            let StateR(x1) = x;
            StateR(s ->
                let (s1, v1) = x1(s);
                (s1, f(v1) as & Pure)
            ) as & ef
    }

    instance Applicative[StateR[s]] {
        pub def point(x: a): StateR[s, a] = StateR(s -> (s, x))

        pub def ap(f: StateR[s, a -> b & ef], x: StateR[s, a]): StateR[s, b] & ef=
            let StateR(f1) = f;
            let StateR(x1) = x;
            StateR(s ->
                /// Eval order is reverse of StateL...
                let (s1, v) = x1(s);
                let (s2, f2) = f1(s1);
                (s2, f2(v) as & Pure)
            ) as & ef
    }

    def runStateR(x: StateR[s, a], state: s): (s, a) =
        let StateR(x1) = x;
        x1(state)

}

/// To be added to List...
namespace ListHelper {
    use ApplicativeExtras.{<*>};
    use FunctorExtras.{<&>};


    def consA(mx: f[a], mxs: f[List[a]]): f[List[a]] with Applicative[f] =
        ((x, xs) -> x :: xs) <&> mx <*> mxs

    pub def traverse(f: a -> m[b] & ef, l: List[a]): m[List[b]] & ef with Applicative[m] =
        def loop(xs, k) = match xs {
            case Nil => k(Applicative.point(Nil))
            case x :: rs => loop(rs, ks -> k(consA(f(x), ks)))
        };
        loop(l, ks -> ks as & ef)


    pub def sequence(l: List[m[a]]): m[List[a]] with Applicative[m] =
        def loop(xs, k) = match xs {
            case Nil => k(Applicative.point(Nil))
            case mx :: rs => loop(rs, ks -> k(consA(mx, ks)))
        };
        loop(l, ks -> ks)

}

/// To be added to Nel...
namespace NelHelper {

    use ApplicativeExtras.{<*>};
    use FunctorExtras.{<&>};

    pub def traverse(f: a -> m[b] & ef, xs: Nel[a]): m[Nel[b]] & ef with Applicative[m] =
        match xs {
            case Nel(x, rs) => ((y, ys) -> Nel(y, ys)) <&> f(x) <*> Traversable.traverse(f, rs)
        }

    pub def sequence(xs: Nel[m[a]]): m[Nel[a]] with Applicative[m] =
        match xs {
            case Nel(x, rs) => ((y, ys) -> Nel(y, ys)) <&> x <*> Traversable.sequence(rs)
        }
}

/// To be added to Chain...
namespace ChainHelper {
    use Applicative.{point};
    use ApplicativeExtras.{<*>};
    use FunctorExtras.{<&>};

    def snocA(mxs: f[Chain[a]], mx: f[a]): f[Chain[a]] with Applicative[f] =
        ((xs, x) -> Chain.snoc(xs, x)) <&> mxs <*> mx

    pub def traverse(f: a -> m[b] & ef, c: Chain[a]): m[Chain[b]] & ef with Applicative[m] =
        def loop(v, acc) = match v {
            case Chain/ViewLeft.NoneLeft        => acc as & ef
            case Chain/ViewLeft.SomeLeft(x, rs) => loop(Chain.viewLeft(rs), snocA(acc, f(x)))
        };
        loop(Chain.viewLeft(c), point(Chain.empty()))

    pub def sequence(c: Chain[m[a]]): m[Chain[a]] with Applicative[m] =
        def loop(v, acc) = match v {
            case Chain/ViewLeft.NoneLeft        => acc
            case Chain/ViewLeft.SomeLeft(x, rs) => loop(Chain.viewLeft(rs), snocA(acc, x))
        };
        loop(Chain.viewLeft(c), point(Chain.empty()))
}

namespace RedBlackTreeHelper {

    use RedBlackTree.RedBlackTree;
    use RedBlackTree.Color;
    use RedBlackTree.RedBlackTree.{Node, Leaf};
    use Applicative.{point};
    use FunctorExtras.{<&>};
    use ApplicativeExtras.{<*>};

    ///
    /// Returns a RedBlackTree with mappings `k -> f(v)` for every `k -> v` in `tree`.
    ///
    pub def mapWithKey(f: (k, v1) -> v2 & ef, tree: RedBlackTree[k, v1]): RedBlackTree[k, v2] & ef =
        mapWithKeyHelper(f, tree, t -> t as & ef)

    def mapWithKeyHelper(f: (k, v1) -> v2 & ef,
                        tree: RedBlackTree[k, v1],
                        k: RedBlackTree[k, v2] -> RedBlackTree[k, v2] & ef): RedBlackTree[k, v2] & ef = match tree {
        case Node(color, left, key, v, right) =>
            mapWithKeyHelper(f, left, kl ->
                mapWithKeyHelper(f, right, kr ->
                    let v1 = f(key, v);
                    k(Node(color, kl, key, v1, kr))))
        case _                              => k(Leaf)
    }

    def nodeA(color: Color, left: m[RedBlackTree[k, v]], k: k, value: m[v], right: m[RedBlackTree[k, v]]): m[RedBlackTree[k, v]] with Applicative[m] =
        (((l, v, r) -> Node(color, l, k, v, r)) <&> left <*> value) <*> right

    ///
    /// Returns a RedBlackTree with mappings `k -> f(v)` for every `k -> v` in `tree`.
    ///
    pub def mapAWithKey(f: (k, v1) -> m[v2] & ef, tree: RedBlackTree[k, v1]): m[RedBlackTree[k, v2]] & ef with Applicative[m] =
        mapAWithKeyHelper(f, tree, t -> t as & ef)

    def mapAWithKeyHelper(f: (k, v1) -> m[v2] & ef,
                        tree: RedBlackTree[k, v1],
                        k: m[RedBlackTree[k, v2]] -> m[RedBlackTree[k, v2]] & ef): m[RedBlackTree[k, v2]] & ef with Applicative[m] =
        match tree {
            case Node(color, left, key, v, right) =>
                mapAWithKeyHelper(f, left, kl ->
                    mapAWithKeyHelper(f, right, kr ->
                        let mv = f(key, v);
                        k(nodeA(color, kl, key, mv, kr))))
            case _                              => k(point(Leaf))
        }

    pub def traverse(f: v1 -> m[v2] & ef, t: RedBlackTree[k, v1]): m[RedBlackTree[k, v2]] & ef  with Applicative[m] =
        mapAWithKey((_, v) -> f(v), t)

    pub def sequence(t: RedBlackTree[k, m[v]]): m[RedBlackTree[k, v]] with Applicative[m] =
        mapAWithKey((_, v) -> v, t)

}


/// To be added to Map...
namespace MapHelper {
    use Applicative.{point};
    use ApplicativeExtras.{<*>};
    use FunctorExtras.{<&>};

    pub def traverse(f: v1 -> m[v2] & ef, m: Map[k, v1]): m[Map[k, v2]] & ef with Applicative[m] =
        let Map(tree) = m;
        Map <&> Traversable.traverse(f, tree)

    pub def traverseWithKey(f: (k, v1) -> m[v2] & ef, m: Map[k, v1]): m[Map[k, v2]] & ef with Applicative[m] =
        let Map(tree) = m;
        Map <&> RedBlackTreeHelper.mapAWithKey(f, tree)

    pub def sequence(m: Map[k, m[v]]): m[Map[k, v]] with Applicative[m] =
        let Map(tree) = m;
        Map <&> Traversable.sequence(tree)

}
