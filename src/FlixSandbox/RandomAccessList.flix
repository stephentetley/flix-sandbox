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


///
/// The RandomAccessList type.
///


namespace FlixSandbox/RandomAccessList {

    enum Tree[a] with Eq, Order, ToString {
        case Leaf(a)
        case Node(a, Tree[a], Tree[a])
    }

    pub enum RandomAccessList[a](List[(Int32, Tree[a])]) with Eq, Order, ToString

    pub enum View[a] with Eq, Order, ToString {
        case RAEmpty
        case RACons(a, RandomAccessList[a])
    }


    type alias RandomAccessListBody[a] = List[(Int32, Tree[a])]
    
    enum FastView[a] with Eq, Order, ToString {
        case FastEmpty
        case FastCons(a, RandomAccessListBody[a])
    }

    def getRandomAccessList(l: RandomAccessList[a]): List[(Int32, Tree[a])] = 
        let RandomAccessList(l1) = l;
        l1

    instance Functor[Tree] {
        pub def map(f: a -> b & ef, t: Tree[a]): Tree[b] & ef = 
            def loop(tree, k) = match tree {
                case Leaf(a)         => {let ans = Leaf(f(a)); k(ans)}
                case Node(a, t1, t2) => {
                    let b = f(a);
                    loop(t1, v1 ->
                        loop(t2, v2 ->
                            k(Node(b, v1, v2))))
                }
            };
            loop(t, k -> k as & ef)
    }

    instance Functor[RandomAccessList] {
        pub def map(f: a -> b & ef, l: RandomAccessList[a]): RandomAccessList[b] & ef = 
            let RandomAccessList(l1) = l;
            Functor.map(x -> {let (i, t) = x; (i, Functor.map(f, t))}, l1) |> RandomAccessList
    }

    pub def empty(): RandomAccessList[a] = RandomAccessList(Nil)
    
    pub def singleton(x: a): RandomAccessList[a] = RandomAccessList((1, Leaf(x)) :: Nil)

    pub def cons(x: a, l: RandomAccessList[a]): RandomAccessList[a] = match getRandomAccessList(l) {
        case (w1, t1) :: (w2, t2) :: rs if (w1 == w2) => RandomAccessList((1 + w1 + w2, Node(x, t1, t2)) :: rs)
        case ts                                       => RandomAccessList((1, Leaf(x)) :: ts)
    }

    pub def head(l: RandomAccessList[a]): Option[a] = match getRandomAccessList(l) {
        case (1, Leaf(x)) :: _          => Some(x)
        case (_, Node(x, _, _)) :: _    => Some(x)
        case _                          => None
    }

    pub def tail(l: RandomAccessList[a]): RandomAccessList[a] = match getRandomAccessList(l) {
        case (1, Leaf(_)) :: ts         => RandomAccessList(ts)
        case (w, Node(_, t1, t2)) :: ts => RandomAccessList((w / 2, t1) :: (w / 2, t2) :: ts)
        case _                          => RandomAccessList(Nil)
    }

    /// Should be tail recursive...
    def lookupTree(weight: Int32, idx: Int32, tree: Tree[a]): Option[a] = match (weight, idx, tree) {
        case (1, 0, Leaf(x))         => Some(x)
        case (_, _, Leaf(_))         => None
        case (_, 0, Node(x, _, _))   => Some(x)
        case (w, i, Node(_, t1, t2)) => 
            if (i <= w/2) 
                lookupTree(w/2, i-1, t1)
            else
                lookupTree(w/2, i - 1 - w / 2, t2)
    }

    pub def get(i: Int32, l: RandomAccessList[a]): Option[a] = 
        def loop(n, body) = match body {
            case Nil          => None
            case (w, t) :: ts => 
                if (i < w)
                    lookupTree(w, n, t)
               else
                    loop(n - w, ts)
        };
        loop(i, getRandomAccessList(l))

    ///
    /// Updates `l` at index `i` with `y`. if `i` is out-of-bounds, returns `l`.
    ///
    pub def update(i: Int32, y: a, l: RandomAccessList[a]): RandomAccessList[a] = 
        let RandomAccessList(l1) = l;
        updateBody(i, y, l1, identity) |> RandomAccessList

    def updateBody(i: Int32, y: a, l: RandomAccessListBody[a], 
                        k: RandomAccessListBody[a] -> RandomAccessListBody[a]): RandomAccessListBody[a] = match l {
        case Nil          => k(Nil)
        case (w, t) :: ts => 
            if (i < w) {
                let t1 = updateTree(w, i, y, t, identity);
                k((w, t1) :: ts)
            } else
                updateBody(i - w, y, ts, ks -> k((w, t) :: ks))
    }

    /// CPS converted
    def updateTree(weight: Int32, idx: Int32, y: a, tree: Tree[a], k: Tree[a] -> Tree[a]): Tree[a] = match (weight, idx, tree) {
        case (1, 0, Leaf(_))         => k(Leaf(y))
        case (_, _, Leaf(x))         => k(Leaf(x))
        case (_, 0, Node(_, t1, t2)) => k(Node(y, t1, t2))
        case (w, i, Node(x, t1, t2)) => 
            if (i <= w/2) 
                updateTree(w/2, i-1, y, t1, kt -> k(Node(x, kt, t2)))
            else
                updateTree(w/2, i - 1 - w / 2, y, t2, kt -> k(Node(x, t1, kt)))
    }

    /// Remove would be an interesting operation (c.f. Map) but it is not supported by List or Array
    /// It is complicated too.


    ///
    /// Updates `l` at index `i` with `y`. if `i` is out-of-bounds, returns `l`.
    ///
    pub def range(b: Int32, e: Int32): RandomAccessList[Int32] = 
        def loop(i, acc) = if (i < b) acc else loop(i-1, cons(i, acc));
        loop(e -1, empty())

    pub def view(l: RandomAccessList[a]): View[a] = 
        let RandomAccessList(l1) = l;
        match l1 {
            case Nil                          => RAEmpty
            case ((_, Leaf(x)) :: ts)         => RACons(x, RandomAccessList(ts))
            case ((w, Node(x, tl, tr)) :: ts) => {
                let hw = w / 2;
                RACons(x, RandomAccessList((hw, tl) :: (hw, tr) :: ts))
            }
        }

    /// Without 1 layer of wrapping...
    def fastView(l: RandomAccessListBody[a]): FastView[a] = 
        match l {
            case Nil                          => FastEmpty
            case ((_, Leaf(x)) :: ts)         => FastCons(x, ts)
            case ((w, Node(x, tl, tr)) :: ts) => {
                let hw = w / 2;
                FastCons(x, (hw, tl) :: (hw, tr) :: ts)
            }
        }

    pub def length(l: RandomAccessList[a]): Int32 = 
        def loop(n, xs) = match xs {
            case Nil            => n
            case ((w, _) :: ts) => loop(n+w, ts)
        };
        let RandomAccessList(l1) = l;
        loop(0, l1)

    pub def foldLeft(f: (b, a) -> b & ef, s: b, l: RandomAccessList[a]): b & ef =
        def loop(s1, xs) = match fastView(xs) {
            case FastEmpty       => s1
            case FastCons(x, rs) => loop(f(s1, x), rs)
        };
        let RandomAccessList(l1) = l;
        loop(s, l1)

    pub def foldRight(f: (a, b) -> b & ef, s: b, l: RandomAccessList[a]): b & ef =
        def loop(xs, k) = match fastView(xs) {
            case FastEmpty       => k(s)
            case FastCons(x, rs) => loop(rs, ka -> k(f(x, ka)))
        };
        let RandomAccessList(l1) = l;
        loop(l1, ka -> ka as & ef)

    pub def filter(f: a -> Bool & ef, l: RandomAccessList[a]): RandomAccessList[a] & ef =
        let step = (x, acc) -> if (f(x)) cons(x, acc) else acc;
        foldRight(step, empty(), l)
    
    pub def filterMap(f: a -> Option[b] & ef, l: RandomAccessList[a]): RandomAccessList[b] & ef =
        let step = (x, acc) -> match f(x) {
            case None    => acc
            case Some(b) => cons(b, acc)
        };
        foldRight(step, empty(), l)

}
