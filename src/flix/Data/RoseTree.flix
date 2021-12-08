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


namespace Data {

    pub enum RoseTree[a] {
        case Node(a, List[RoseTree[a]])
    }

    instance Eq[RoseTree[a]] with Eq[a] {
        pub def eq(t1: RoseTree[a], t2: RoseTree[a]): Bool = Data/RoseTree.equals(t1, t2)
    }

    instance Functor[RoseTree] {
        pub def map(f: a -> b & ef, t: RoseTree[a]): RoseTree[b] & ef = Data/RoseTree.map(f, t)
    }


    pub type alias RoseForest[a] = List[RoseTree[a]]
}

namespace Data/RoseTree {

    use Data.{RoseTree, RoseForest};
    use Data.RoseTree.{Node};

    ///
    /// Returns `true` if and only if `c1` and `c2` and equal.
    ///
    pub def equals(t1: RoseTree[a], t2: RoseTree[a]): Bool with Eq[a] = match (t1, t2) {
        case (Node(a, xs), Node(b, ys)) => if (a == b) equalsHelper(xs, ys) else false
    }

    def equalsHelper(xs: RoseForest[a], ys: RoseForest[a]): Bool with Eq[a] = match (xs, ys) {
            case (Nil, Nil)          => true
            case (a :: rs1, b:: rs2) => if (a == b) equalsHelper(rs1, rs2) else false
            case _                   => false
        }

    pub def drawTree(x: RoseTree[String]): String = drawTree1(x, xs -> xs) |> String.unlines

    pub def drawForest(x: RoseForest[String]): String = String.unlines(List.map(drawTree, x))

    def shift1(first: String, other: String, xs: List[String], k: List[String] -> List[String]): List[String] = 
        match xs { 
            case Nil => k(Nil)
            case x :: rs => { 
                let s1 = first + x;
                shift2(other, rs, ss -> k(s1 :: ss))
            }
        }
    
    def shift2(other: String, xs: List[String], k: List[String] -> List[String]): List[String] = 
        match xs {
            case Nil => k(Nil)
            case x :: rs => {
                let s1 = other + x;
                shift2(other, rs, ss -> k(s1 :: ss))
            }
        }            


    def drawTree1(x: RoseTree[String], k: List[String] -> List[String]): List[String] = 
        match x {
            case Node(label, kids) => {
                let xs = String.lines(label);
                drawKids(kids, ys -> k(xs ::: ys))
            }
        }

    def drawKids(xs: List[RoseTree[String]], k: List[String] -> List[String]): List[String] = 
        match xs {
            case Nil       => k(Nil)
            case t1 :: Nil => drawTree1(t1, ac1 -> shift1("`- ", "   ", ac1, ac2 -> k("|" :: ac2)))
            case t1 :: rs  =>  drawTree1(t1, ac1 -> shift1("+- ", "|  ", ac1, ac2 -> drawKids(rs, ac3 -> k("|" :: (ac2 ::: ac3)))))
        }



    pub def flatten(x: RoseTree[a]): List[a] = preorder(x)

    pub def preorder(x: RoseTree[a]): List[a] =
        preorderHelper(x, identity) |> Chain.toList

    def preorderHelper(x: RoseTree[a], k: Chain[a] -> Chain[a]): Chain[a] = match x {
        case Node(a, Nil) => k(Chain.singleton(a))
        case Node(a, xs)  => preorderForest(xs, vs -> k(Chain.cons(a, vs)))
    }

    def preorderForest(xs: RoseForest[a], k: Chain[a] -> Chain[a]): Chain[a] = match xs {
        case Nil     => k(Chain.empty())
        case x :: rs => preorderHelper(x, vs1 -> preorderForest(rs, vs2 -> k(Chain.append(vs1, vs2))))
    }

    pub def postorder(x: RoseTree[a]): List[a] =
        postorderHelper(x, identity) |> Chain.toList


    def postorderHelper(x: RoseTree[a], k: Chain[a] -> Chain[a]): Chain[a] = match x {
        case Node(a, Nil) => k(Chain.singleton(a))
        case Node(a, xs)  => postorderForest(xs, vs -> k(Chain.snoc(vs, a)))
    }

    def postorderForest(xs: RoseForest[a], k: Chain[a] -> Chain[a]): Chain[a] = match xs {
        case Nil => k(Chain.empty())
        case x :: rs => postorderHelper(x, vs1 -> postorderForest(rs, vs2 -> k(Chain.append(vs1, vs2))))
    }


    ///
    /// Tree catamorphism
    ///
    pub def foldRoseTree(f: (a, List[b]) -> b, t: RoseTree[a]): b =
        foldRoseTreeHelper(f, t, identity)

    ///
    /// Helper for `foldRoseTree` in CPS.
    ///
    def foldRoseTreeHelper(f: (a, List[b]) -> b, t: RoseTree[a], k: b -> b): b = match t {
        case Node(x,kids) => foldRoseForest(f, kids, vs -> k(f(x,vs)))
    }

    ///
    /// Helper for `foldRoseTree` in CPS.
    ///
    def foldRoseForest(f: (a, List[b]) -> b, ts: RoseForest[a], k: List[b] -> b): b = match ts {
        case Nil => k(Nil)
        case x :: rs => foldRoseTreeHelper(f, x, v -> foldRoseForest(f, rs, vs -> k(v :: vs)))
    }

    pub def map(f: a -> b & ef, t: RoseTree[a]): RoseTree[b] & ef =
        mapHelper(f, t, k -> k as & ef)

    def mapHelper(f: a -> b & ef, t: RoseTree[a], k: RoseTree[b] -> RoseTree[b] & ef): RoseTree[b] & ef = match t {
        case Node(a,xs) => mapForest(f, xs, vs -> k(Node(f(a), vs)))
    }

    def mapForest(f: a -> b & ef, ts: RoseForest[a], k: RoseForest[b] -> RoseTree[b] & ef): RoseTree[b] & ef = match ts {
        case Nil     => k(Nil)
        case x :: rs => mapHelper(f, x, v1 -> mapForest(f, rs, vs -> k(v1 :: vs)))
    }

    /// Traversal order is preorder (check...)
    pub def foreach(f: a -> Unit & ef, t: RoseTree[a]): Unit & ef =
        let Node(a,xs) = t;
        f(a);
        foreachForest(f, xs)

    def foreachForest(f: a -> Unit & ef, ts: RoseForest[a]): Unit & ef = match ts {
        case Nil => ()
        case x :: rs => { foreach(f, x); foreachForest(f, rs) }
    }

    pub def unfoldTree(f: b -> (a, List[b]) & ef, st: b): RoseTree[a] & ef=
        unfoldTreeHelper(f, st, k -> k as & ef)

    /// CPS
    def unfoldTreeHelper(f: b -> (a, List[b]) & ef, st: b, k: RoseTree[a] -> RoseTree[a] & ef): RoseTree[a] & ef =
        let (a, bs) = f(st);
        unfoldTreeListHelper(f, bs, ks -> k(Node(a, ks)))

    /// CPS
    def unfoldTreeListHelper(f: b -> (a, List[b]) & ef, ss: List[b], k: RoseForest[a] -> RoseTree[a] & ef): RoseTree[a] & ef =
        match ss {
            case Nil => k(Nil)
            case s :: rs => unfoldTreeHelper(f, s, k1 ->
                                unfoldTreeListHelper(f, rs, ks -> k(k1 :: ks)))
        }


    pub def unfoldForest(f: b -> (a, List[b]) & ef, ss: List[b]): RoseForest[a] & ef =
        List.map(unfoldTree(f), ss)


    pub def traverse(f: a -> m[b] & ef, t: RoseTree[a]): m[RoseTree[b]] & ef with Applicative[m] =
        traverseTree(f, t)

    pub def sequence(t: RoseTree[m[a]]): m[RoseTree[a]] with Applicative[m] =
        traverse(identity, t)

    /// TODO cps transform...
    def traverseTree(f: a -> m[b] & ef, t: RoseTree[a]): m[RoseTree[b]] & ef with Applicative[m] =
        use ApplicativeExtras.{<*>};
        use FunctorExtras.{<&>};
        match t {
            case Node(x, xs) => ((y, ys) -> Node(y, ys)) <&> f(x) <*> traverseForest(f, xs)
        }


    def consA(mx: f[a], mxs: f[List[a]]): f[List[a]] with Applicative[f] =
        use ApplicativeExtras.{<*>};
        use FunctorExtras.{<&>};
        ((x, xs) -> x :: xs) <&> mx <*> mxs

    /// TODO cps transform...
    def traverseForest(f: a -> m[b] & ef, l: RoseForest[a]): m[RoseForest[b]] & ef with Applicative[m] =
        def loop(xs, k) = match xs {
            case Nil => k(Applicative.point(Nil))
            case x :: rs => loop(rs, ks -> k(consA(traverseTree(f, x), ks)))
        };
        loop(l, ks -> ks as & ef)

}