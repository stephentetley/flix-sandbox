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

pub lawless class Traversable[t : Type -> Type] with Functor[t] {

    pub def traverse(f: a -> m[b] & ef, c: t[a]): m[t[b]] & ef with Applicative[m]

    pub def sequence(c: t[m[a]]): m[t[a]] with Applicative[m]

    pub def mapM(f: a -> m[b] & ef, c: t[a]): m[t[b]] & ef with Monad[m]

    /// This would be better placed in Foldable...
    pub def foreachM(f: a -> m[b] & ef, c: t[a]): m[Unit] & ef with Monad[m]

}

/// Add instances for Nel, Chain


instance Traversable[List] {
    /// CPS transformed...
    pub def traverse(f: a -> m[b] & ef, t: List[a]): m[List[b]] & ef with Applicative[m] = ListHelper.traverse(f, t)

    pub def sequence(t: List[m[a]]): m[List[a]] with Applicative[m] = ListHelper.sequence(t)

    pub def mapM(f: a -> m[b] & ef, t: List[a]): m[List[b]] & ef with Monad[m] = ListHelper.mapM(f, t)

    pub def foreachM(f: a -> m[b] & ef, t: List[a]): m[Unit] & ef with Monad[m] = ListHelper.foreachM(f, t)

}

instance Traversable[Nel] {
    /// CPS transformed...
    pub def traverse(f: a -> m[b] & ef, t: Nel[a]): m[Nel[b]] & ef with Applicative[m] = NelHelper.traverse(f, t)

    pub def sequence(t: Nel[m[a]]): m[Nel[a]] with Applicative[m] = NelHelper.sequence(t)

    pub def mapM(f: a -> m[b] & ef, t: Nel[a]): m[Nel[b]] & ef with Monad[m] = NelHelper.mapM(f, t)

    pub def foreachM(f: a -> m[b] & ef, t: Nel[a]): m[Unit] & ef with Monad[m] = NelHelper.foreachM(f, t)

}

instance Traversable[Chain] {
    /// CPS transformed...
    pub def traverse(f: a -> m[b] & ef, t: Chain[a]): m[Chain[b]] & ef with Applicative[m] = ChainHelper.traverse(f, t)

    pub def sequence(t: Chain[m[a]]): m[Chain[a]] with Applicative[m] = ChainHelper.sequence(t)

    pub def mapM(f: a -> m[b] & ef, t: Chain[a]): m[Chain[b]] & ef with Monad[m] = ChainHelper.mapM(f, t)

    pub def foreachM(f: a -> m[b] & ef, t: Chain[a]): m[Unit] & ef with Monad[m] = ChainHelper.foreachM(f, t)

}

instance Traversable[RedBlackTree.RedBlackTree[k]] {
    pub def traverse(f: a -> m[b] & ef, t: RedBlackTree.RedBlackTree[k, a]): m[RedBlackTree.RedBlackTree[k, b]] & ef with Applicative[m] = RedBlackTreeHelper.traverse(f, t)

    pub def sequence(t: RedBlackTree.RedBlackTree[k, m[a]]): m[RedBlackTree.RedBlackTree[k, a]] with Applicative[m] = RedBlackTreeHelper.sequence(t)

    pub def mapM(f: a -> m[b] & ef, t: RedBlackTree.RedBlackTree[k, a]): m[RedBlackTree.RedBlackTree[k, b]] & ef with Monad[m] = RedBlackTreeHelper.mapM(f, t)

    pub def foreachM(f: a -> m[b] & ef, t: RedBlackTree.RedBlackTree[k, a]): m[Unit] & ef with Monad[m] = RedBlackTreeHelper.foreachM(f, t)
}


instance Traversable[Map[k]] {
    pub def traverse(f: a -> m[b] & ef, t: Map[k, a]): m[Map[k, b]] & ef with Applicative[m] = MapHelper.traverse(f, t)

    pub def sequence(t: Map[k, m[a]]): m[Map[k, a]] with Applicative[m] = MapHelper.sequence(t)

    pub def mapM(f: a -> m[b] & ef, t: Map[k, a]): m[Map[k, b]] & ef with Monad[m] = MapHelper.mapM(f, t)

    pub def foreachM(f: a -> m[b] & ef, t: Map[k, a]): m[Unit] & ef with Monad[m] = MapHelper.foreachM(f, t)
}

instance Traversable[Option] {
    pub def traverse(f: a -> m[b] & ef, t: Option[a]): m[Option[b]] & ef with Applicative[m] =
        use Applicative.{point};
        use Functor.{map};
        match t {
            case None => point(None)
            case Some(a) => f(a) |> map(Some)
        }

    pub def sequence(t: Option[m[a]]): m[Option[a]] with Applicative[m] =
        use Applicative.{point};
        use Functor.{map};
        match t {
            case Some(ma) => map(Some, ma)
            case None => point(None)
        }

    pub def mapM(f: a -> m[b] & ef, t: Option[a]): m[Option[b]] & ef with Monad[m] =
        use Applicative.{point};
        use Functor.{map};
        match t {
            case None => point(None)
            case Some(a) => f(a) |> map(Some)
        }

    pub def foreachM(f: a -> m[b] & ef, t: Option[a]): m[Unit] & ef with Monad[m] =
        use Applicative.{point};
        match t {
            case None => point(())
            case Some(a) => Monad.flatMap(_ -> point(()), f(a))
        }
}

namespace Traversable {

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

    pub def traverse(f: a -> m[b] & ef, xs: List[a]): m[List[b]] & ef with Applicative[m] =
        traverseHelper(f, xs, ks -> ks as & ef)

    def traverseHelper(f: a -> m[b] & ef, xs: List[a], k: m[List[b]] -> m[List[b]] & ef): m[List[b]] & ef with Applicative[m] =
        match xs {
            case Nil => k(Applicative.point(Nil)) as & ef
            case x :: rs => traverseHelper(f, rs, ks -> k(consA(f(x), ks)))
        }

    def consA(mx: f[a], mxs: f[List[a]]): f[List[a]] with Applicative[f] =
        ((x, xs) -> x :: xs) <&> mx <*> mxs


    pub def sequence(xs: List[m[a]]): m[List[a]] with Applicative[m] = sequenceHelper(xs, ks -> ks)

    def sequenceHelper(xs: List[m[a]], k: m[List[a]] -> m[List[a]]): m[List[a]] with Applicative[m] =
        match xs {
            case Nil => k(Applicative.point(Nil))
            case mx :: rs => sequenceHelper(rs, ks -> k(consA(mx, ks)))
        }

    pub def sequenceX(xs: List[m[a]]): m[Unit] with Applicative[m] = sequenceXHelper(xs, k -> k)

    def sequenceXHelper(xs: List[m[a]], k: m[Unit] -> m[Unit]): m[Unit] with Applicative[m] =
        use ApplicativeExtras.{seqRight};
        match xs {
            case Nil => k(Applicative.point(()))
            case mx :: rs => sequenceXHelper(rs, ka -> k(seqRight(mx, ka)))
        }

    pub def mapM(f: a -> m[b] & ef, xs: List[a]): m[List[b]] & ef with Monad[m] =
        mapMHelper(f, xs, ks -> ks as & ef)

    def mapMHelper(f: a -> m[b] & ef, xs: List[a], k: m[List[b]] -> m[List[b]] & ef): m[List[b]] & ef with Monad[m] =
        match xs {
            case Nil => k(Applicative.point(Nil)) as & ef
            case x :: rs => mapMHelper(f, rs, ks -> k(consA(f(x), ks)))
        }

    pub def foreachM(f: a -> m[b] & ef, xs: List[a]): m[Unit] & ef with Monad[m] =
        foreachMHelper(f, xs, ku -> ku as & ef)

    pub def foreachMHelper(f: a -> m[b] & ef, xs: List[a], k: m[Unit] -> m[Unit] & ef): m[Unit] & ef with Monad[m] =
        match xs {
            case Nil => k(Applicative.point(())) as & ef
            case x :: rs => foreachMHelper(f, rs, ku -> k(Monad.flatMap(_ -> ku, f(x))))
        }

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

    pub def mapM(f: a -> m[b] & ef, xs: Nel[a]): m[Nel[b]] & ef with Monad[m] =
        match xs {
            case Nel(x, rs) => ((y, ys) -> Nel(y, ys)) <&> f(x) <*> Traversable.mapM(f, rs)
        }

    pub def foreachM(f: a -> m[b] & ef, xs: Nel[a]): m[Unit] & ef with Monad[m] =
        match xs {
            case Nel(x, rs) => ((_, ac) -> ac) <&> f(x) <*> Traversable.foreachM(f, rs)
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
        traverseHelper(f, Chain.viewLeft(c), point(Chain.empty()))

    def traverseHelper(f: a -> m[b] & ef, v: Chain.ViewLeft[a], acc: m[Chain[b]]): m[Chain[b]] & ef with Applicative[m] =
        match v {
            case Chain/ViewLeft.NoneLeft        => acc as & ef
            case Chain/ViewLeft.SomeLeft(x, rs) => traverseHelper(f, Chain.viewLeft(rs), snocA(acc, f(x)))
        }

    pub def sequence(c: Chain[m[a]]): m[Chain[a]] with Applicative[m] =
        sequenceHelper(Chain.viewLeft(c), point(Chain.empty()))

    def sequenceHelper(v: Chain.ViewLeft[m[a]], acc: m[Chain[a]]): m[Chain[a]] with Applicative[m] =
        match v {
            case Chain/ViewLeft.NoneLeft        => acc
            case Chain/ViewLeft.SomeLeft(x, rs) => sequenceHelper(Chain.viewLeft(rs), snocA(acc, x))
        }

    pub def mapM(f: a -> m[b] & ef, c: Chain[a]): m[Chain[b]] & ef with Monad[m] =
        mapMHelper(f, Chain.viewLeft(c), point(Chain.empty()))

    def mapMHelper(f: a -> m[b] & ef, v: Chain.ViewLeft[a], acc: m[Chain[b]]): m[Chain[b]] & ef with Monad[m] =
        match v {
            case Chain/ViewLeft.NoneLeft        => acc
            case Chain/ViewLeft.SomeLeft(x, rs) => mapMHelper(f, Chain.viewLeft(rs), snocA(acc, f(x)))
        }

    pub def foreachM(f: a -> m[b] & ef, c: Chain[a]): m[Unit] & ef with Monad[m] =
        foreachMHelper(f, Chain.viewLeft(c), point(()))

    pub def foreachMHelper(f: a -> m[b] & ef, v: Chain.ViewLeft[a], acc: m[Unit]): m[Unit] & ef with Monad[m] =
        match v {
            case Chain/ViewLeft.NoneLeft        => acc as & ef
            case Chain/ViewLeft.SomeLeft(x, rs) => foreachMHelper(f, Chain.viewLeft(rs), Monad.flatMap(_ -> acc, f(x)))
        }
}

namespace RedBlackTreeHelper {

    use RedBlackTree.RedBlackTree;
    use RedBlackTree.Color;
    use RedBlackTree.RedBlackTree.{Node, Leaf};
    use Applicative.{point};
    use FunctorExtras.{<&>};
    use ApplicativeExtras.{<*>};


    def nodeA(color: Color, left: m[RedBlackTree[k, v]], k: k, value: m[v], right: m[RedBlackTree[k, v]]): m[RedBlackTree[k, v]] with Applicative[m] =
        (((l, v, r) -> Node(color, l, k, v, r)) <&> left <*> value) <*> right

    ///
    /// Returns a RedBlackTree with mappings `k -> f(v)` for every `k -> v` in `tree`.
    ///
    /// Not tail-recursive!
    pub def mapAWithKey(f: (k, v1) -> m[v2] & ef, tree: RedBlackTree[k, v1]): m[RedBlackTree[k, v2]] & ef  with Applicative[m] = match tree {
        case Node(color, left, k, v, right) => nodeA(color, mapAWithKey(f, left), k, f(k, v), mapAWithKey(f, right))
        case _                              => point(Leaf)
    }

    pub def traverse(f: v1 -> m[v2] & ef, t: RedBlackTree[k, v1]): m[RedBlackTree[k, v2]] & ef  with Applicative[m] =
        mapAWithKey((_, v) -> f(v), t)

    pub def sequence(t: RedBlackTree[k, m[v]]): m[RedBlackTree[k, v]] with Applicative[m] =
        mapAWithKey((_, v) -> v, t)

    pub def mapM(f: v1 -> m[v2] & ef, t: RedBlackTree[k, v1]): m[RedBlackTree[k, v2]] & ef with Monad[m] =
        mapAWithKey((_, v) -> f(v), t)

    pub def foreachM(f: v1 -> m[v2] & ef, t: RedBlackTree[k, v1]): m[Unit] & ef with Monad[m] =
        RedBlackTree.foldLeft((acc, _, v) -> (((_, acc1) -> acc1) <&> f(v) <*> acc), point(()), t)

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

    pub def mapM(f: v1 -> m[v2] & ef, m: Map[k, v1]): m[Map[k, v2]] & ef with Monad[m] =
        let Map(tree) = m;
        Map <&> Traversable.mapM(f, tree)

    pub def mapMWithKey(f: (k, v1) -> m[v2] & ef, m: Map[k, v1]): m[Map[k, v2]] & ef with Monad[m] =
        let Map(tree) = m;
        Map <&> RedBlackTreeHelper.mapAWithKey(f, tree)

    pub def foreachM(f: v1 -> m[v2] & ef, m: Map[k, v1]): m[Unit] & ef with Monad[m] =
        let Map(tree) = m;
        Traversable.foreachM(f, tree)

    pub def foreachMWithKey(f: (k, v1) -> m[v2] & ef, m: Map[k, v1]): m[Unit] & ef with Monad[m] =
        Map.foldLeftWithKey((acc, k, v) -> (((_, acc1) -> acc1) <&> f(k, v) <*> acc), point(()), m)

}