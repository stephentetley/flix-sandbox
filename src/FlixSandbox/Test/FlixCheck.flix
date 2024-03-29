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

/// ##java.util.SplittableRandom introduces a compatibility error in Java 17
/// For the time being this is broken.

/*

namespace FlixSandbox/Test/FlixCheck {

    ///
    /// Represents a random number generator.
    ///
    pub enum SplittableRandom(##java.util.SplittableRandom)

    def splittableRandom(): SplittableRandom & Impure =
        import new java.util.SplittableRandom() as newSplittableRandom;
        SplittableRandom(newSplittableRandom())

    // def nextInt(r: Random, max: Int32): Int32 & Impure =
    //     import java.util.SplittableRandom.nextInt(Int32);
    //     let Random(r1) = r;
    //     nextInt(r1, max)

    // def nextLong(r: Random, max: Int64): Int64 & Impure =
    //     import java.util.SplittableRandom.nextLong(Int64);
    //     let Random(r1) = r;
    //     nextLong(r1, max)

    def split(r: SplittableRandom): SplittableRandom & Impure =
        import java.util.SplittableRandom.split();
        let SplittableRandom(r1) = r;
        SplittableRandom(split(r1))

    def split2(r: SplittableRandom): (SplittableRandom, SplittableRandom) & Impure =
        let rx = split(r);
        let ry = split(rx);
        (rx, ry)



    def randomR(lo: Int32, hi: Int32, r: SplittableRandom): Int32 & Impure =
        import java.util.random.RandomGenerator.nextInt(Int32, Int32);
        let SplittableRandom(r1) = r;
        nextInt(r1, lo, hi)

    def randomR64(lo: Int64, hi: Int64, r: SplittableRandom): Int64 & Impure =
        import java.util.random.RandomGenerator.nextLong(Int64, Int64);
        let SplittableRandom(r1) = r;
        nextLong(r1, lo, hi)

    def stringCompare(s: String, t: String): Comparison =
        Order.compare(s, t)

    def listCompareWith(cmp: (a,a) -> Comparison, xs: List[a], ys: List[a]): Comparison =
        match (xs,ys) {
            case (Nil, Nil)             => EqualTo
            case (Nil, _)               => LessThan
            case (_, Nil)               => GreaterThan
            case (x :: xs1, y :: ys1)   => {
                let c = cmp(x,y);
                if (c == EqualTo)
                    listCompareWith(cmp, xs1, ys1)
                else
                    c
                }
        }


    pub enum Gen[a]((Int32, SplittableRandom) -> a & Impure)


    instance Functor[Gen] {
        pub def map(f: a -> b & ef, x: Gen[a]): Gen[b] & ef = map(f, x)
    }

    instance Applicative[Gen] {
        pub def point(x: a): Gen[a] = point(x)

        pub def ap(f: Gen[a -> b & ef], x: Gen[a]): Gen[b] & ef = ap(f, x)
    }


    pub lawless class Arbitrary[a] {
        pub def arbitrary(): Gen[a]
    }

    pub lawless class Coarbitrary[a] {
        pub def coarbitrary(x: a, ma: Gen[b]): Gen[b]
    }

    instance Arbitrary[Unit] {
        pub def arbitrary(): Gen[Unit] = point()
    }

    instance Coarbitrary[Unit] {
        pub def coarbitrary(_: Unit, ma: Gen[b]): Gen[b] = ma
    }

    instance Arbitrary[Bool] {
        pub def arbitrary(): Gen[Bool] =
            let* arr = liftAction(_ -> [true, false]);
            elements(arr)
    }

    instance Coarbitrary[Bool] {
        pub def coarbitrary(b: Bool, ma: Gen[b]): Gen[b] = variant(if (b) 0 else 1, ma)
    }

    instance Arbitrary[Int32] {
            pub def arbitrary(): Gen[Int32] = sized(n -> choose(-n, n))
    }


    instance Coarbitrary[Int32] {
        pub def coarbitrary(n: Int32, ma: Gen[b]): Gen[b] = coarbitraryInt32(n, ma)
    }

    instance Arbitrary[Int64] {
        pub def arbitrary(): Gen[Int64] = sized(n -> { let x = Int32.toInt64(n); chooseInt64(-x, x + 1i64) })
    }

    instance Coarbitrary[Int64] {
        pub def coarbitrary(n: Int64, ma: Gen[b]): Gen[b] = coarbitraryInt64(n, ma)
    }

    instance Arbitrary[(a1, a2)] with Arbitrary[a1], Arbitrary[a2] {
        pub def arbitrary(): Gen[(a1, a2)] =
            Applicative.liftA2((x, y) -> (x, y), Test/FlixCheck/Arbitrary.arbitrary(), Test/FlixCheck/Arbitrary.arbitrary())
    }

    instance Coarbitrary[(a1, a2)] with Coarbitrary[a1], Coarbitrary[a2] {
        pub def coarbitrary(x: (a1, a2), ma: Gen[b]): Gen[b] =
            let (a, b) = x;
            Test/FlixCheck/Coarbitrary.coarbitrary(a, Test/FlixCheck/Coarbitrary.coarbitrary(b, ma))
    }

    instance Arbitrary[(a1, a2, a3)] with Arbitrary[a1], Arbitrary[a2], Arbitrary[a3] {
        pub def arbitrary(): Gen[(a1, a2, a3)] =
            Applicative.liftA3((x, y, z) -> (x, y, z),
                                Test/FlixCheck/Arbitrary.arbitrary(),
                                Test/FlixCheck/Arbitrary.arbitrary(),
                                Test/FlixCheck/Arbitrary.arbitrary())
    }

    instance Coarbitrary[(a1, a2, a3)] with Coarbitrary[a1], Coarbitrary[a2], Coarbitrary[a3] {
        pub def coarbitrary(x: (a1, a2, a3), ma: Gen[b]): Gen[b] =
            let (a, b, c) = x;
            Test/FlixCheck/Coarbitrary.coarbitrary(a,
                Test/FlixCheck/Coarbitrary.coarbitrary(b,
                    Test/FlixCheck/Coarbitrary.coarbitrary(c, ma)))
    }

    instance Arbitrary[List[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[List[a]] =
            sized(n -> bind(choose(0, n), list))
    }

    /// TODO make tail recursive
    instance Coarbitrary[List[a]] with Coarbitrary[a] {
        pub def coarbitrary(l: List[a], ma: Gen[b]): Gen[b] = match l {
            case Nil => variant(0, ma)
            case x :: rs => variant(1, Test/FlixCheck/Coarbitrary.coarbitrary(x, Test/FlixCheck/Coarbitrary.coarbitrary(rs, ma)))
        }
    }

    instance Arbitrary[Array[a]] with Arbitrary[a] {
        pub def arbitrary(): Gen[Array[a]] =
            sized(n -> bind(choose(0, n), vector))
    }

    /// TODO - does this need to be tail recursive?
    def coarbitraryInt32(n: Int32, ma: Gen[b]): Gen[b] = match n {
        case i if i == 0 => variant(0, ma)
        case i if i < 0  => variant(2, (coarbitraryInt32(-n, ma)))
        case _           => variant(1, (coarbitraryInt32(n / 2, ma)))
    }

    def coarbitraryInt64(n: Int64, ma: Gen[b]): Gen[b] = match n {
        case i if i == 0i64 => variant(0, ma)
        case i if i < 0i64  => variant(2, (coarbitraryInt64(-n, ma)))
        case _              => variant(1, (coarbitraryInt64(n / 2i64, ma)))
    }


    pub def run(x: Gen[a], bounds: Int32): a & Impure =
        let r1 = newRandom();
        apply1(x, bounds, r1)


    pub def point(x: a): Gen[a] = Gen((_,_) -> x as & Impure)

    def apply1(x: Gen[a], n: Int32, r: SplittableRandom): a & Impure =
        let Gen(f) = x;
        f(n, r)

    pub def map(f: a -> b & ef, x: Gen[a]): Gen[b] & ef =
        Gen((b,r) ->
            let a = apply1(x, b, r);
            f(a) as & Impure
        ) as & ef


    pub def ap(mf: Gen[a -> b & ef], ma: Gen[a]): Gen[b] & ef =
        Gen((b,r) ->
            let f = apply1(mf, b, r);
            let a = apply1(ma, b, r);
            f(a) as & Impure
        ) as & ef

    pub def bind(x: Gen[a], k: a -> Gen[b] & ef): Gen[b] & ef=
        Gen((b,r) ->
            let a = apply1(x, b, r);
            let y = k(a) as & Impure;
            apply1(y, b, r)
        ) as & ef

    pub def flatMap(k: a -> Gen[b] & ef, x: Gen[a]): Gen[b] & ef = bind(x, k)

    pub def liftAction(f: Unit -> a & Impure): Gen[a] =
        Gen((_,_) -> f())


    pub def sequence(xs: List[Gen[a]]): Gen[List[a]] = sequenceHelper(xs, point)

    pub def sequenceHelper(xs: List[Gen[a]], k: List[a]-> Gen[List[a]]): Gen[List[a]] =
        match xs {
            case Nil => k(Nil)
            case xf :: rs => {
                let* x = xf;
                sequenceHelper(rs, xs -> k(x :: xs))
            }
        }

    pub def sized(fgen: Int32 -> Gen[a]): Gen[a] =
        Gen((b,r) ->
            let Gen(m) = fgen(b);
            m(b, r)
        )

    pub def resize(bound: Int32, x:Gen[a]): Gen[a] =
        Gen((_,r) -> apply1(x, bound, r))

    pub def rand(): Gen[SplittableRandom] =
        Gen((_,r) -> r as & Impure)


    /// Promote is difficult to type in presence of effects.
    /// See all the casting!
    pub def promote(f: a -> Gen[b] & ef): Gen[a -> b & ef] =
        Gen((n, r) ->
            let f1 = a -> {
                let mf: Gen[b] = f(a) as & Pure;
                apply1(mf, n, r) as & ef
                };
            f1 as & Impure
        )

    pub def generate(n: Int32, rnd: SplittableRandom, gen: Gen[a]): a & Impure =
        let size = randomR(0, Int32.abs(n), rnd);   /// TODO remove abs
        apply1(gen, size, rnd)


    pub def choose(lo:Int32, hi: Int32): Gen[Int32] =
        let* r1 = rand();
        let* v = liftAction(_ -> randomR(lo, hi, r1));
        point(v)

    pub def variant(v: Int32, m: Gen[a]): Gen[a] =
        Gen((n,r) ->
            let Gen(mf) = m;
            mf(n, makeRands(r, v))
        )

    def makeRands(r0: SplittableRandom, n: Int32): SplittableRandom & Impure =
        if (n <= 0)
            r0
        else {
            let (r1, r2) = split2(r0);
            let n1 = n / 2;
            let s = n `Mod.mod` 2;
            if (s == 0)
                makeRands(r1, n1)
            else
                makeRands(r2, n1)

        }


    def chooseInt64(lo:Int64, hi: Int64): Gen[Int64] =
        let* r1 = rand();
        let* v = liftAction(_ -> randomR64(lo, hi, r1));
        point(v)

    pub def elements(arr: Array[a]): Gen[a] =
        let hi = Array.length(arr);
        let* ix = choose(0, hi);
        let* v = liftAction(_ -> arr[ix]);
        point(v)

    pub def vector(n: Int32): Gen[Array[a]] with Arbitrary[a] =
        Gen((b,r) ->
            let f = _ -> apply1(Test/FlixCheck/Arbitrary.arbitrary(), b, r);
            Array.init(f, n)
        )

    pub def list(n: Int32): Gen[List[a]] with Arbitrary[a] =
        if (n <= 0)
            point(Nil)
        else
            listHelper(Test/FlixCheck/Arbitrary.arbitrary(), n)

    def listHelper(arb: Gen[a], n: Int32): Gen[List[a]] =
        Gen((b,r) ->
            let f = _ -> apply1(arb, b, r);
            let arr = Array.init(f, n);
            Array.toList(arr)
        )



    pub def oneof(arr: Array[Gen[a]]): Gen[a] = flatMap(x -> x, elements(arr))



    ///
    /// Returns the absolute value of `x`.
    /// If the absolute value exceeds maxValue(), -1 is returned.
    ///
    def absF32(x: Float32): Float32 = {
        if      (x >= 0.0f32)               x
        else if (x == Float32.minValue())   -1.0f32
        else                                -x
    }

    def fractionF32 (a: Int32, b: Int32, c: Int32): Float32 =
        use Int32.{toFloat32};
        toFloat32(a) + (toFloat32(b) / (absF32(toFloat32(c)) + 1.0f32))

    pub def genFloat32(): Gen[Float32] =
        let* a: Int32 = Test/FlixCheck/Arbitrary.arbitrary();
        let* b: Int32 = Test/FlixCheck/Arbitrary.arbitrary();
        let* c: Int32 = Test/FlixCheck/Arbitrary.arbitrary();
        point(fractionF32(a,b,c))

    ///
    /// Returns the absolute value of `x`.
    /// If the absolute value exceeds maxValue(), -1 is returned.
    ///
    def absF64(x: Float64): Float64 = {
        if      (x >= 0.0f64)               x
        else if (x == Float64.minValue())   -1.0f64
        else                                -x
    }

    def fractionF64 (a: Int32, b: Int32, c: Int32): Float64 =
        use Int32.{toFloat64};
        toFloat64(a) + (toFloat64(b) / (absF64(toFloat64(c)) + 1.0f64))

    pub def genFloat64(): Gen[Float64] =
        let* a: Int32 = Test/FlixCheck/Arbitrary.arbitrary();
        let* b: Int32 = Test/FlixCheck/Arbitrary.arbitrary();
        let* c: Int32 = Test/FlixCheck/Arbitrary.arbitrary();
        point(fractionF64(a,b,c))



    type alias Answer = { okay :: Option[Bool], stamp :: List[String], arguments :: List[String] }

    def nothing(): Answer = { okay = None, stamp = Nil, arguments = Nil }

    pub enum Property(Gen[Answer])

    pub def answer(ans: Answer): Property =
        let ma = point(ans);
        Property(ma)

    pub def evaluate(testable: a -> Property, x: a): Gen[Answer] =
        let Property(ma) = testable(x);
        ma

    pub def testableUnit(): Property = answer(nothing())

    pub def testableBool(x: Bool): Property =
        let ans = nothing();
        answer( {  okay = Some(x) | ans })

    pub def testableAnswer(ans: Answer): Property = answer(ans)

    pub def testableProperty(x: Property): Property = x

    pub def testableFunction(dict: {show :: a -> String, testable :: b -> Property, arbitrary :: Gen[a]}, f: a -> b): Property =
        let dminus = {show = dict.show, testable = dict.testable};
        forall(dminus, dict.arbitrary, f)

    pub def forall(dict: {show :: a -> String, testable :: b -> Property}, gen: Gen[a], body: a -> b): Property =
        let argument = (a,ans) -> {
            let show1 = dict.show;
            let args = show1(a) :: ans.arguments;
            { arguments = args | ans }
        };
        Property(
            let* a = gen;
            let* ans = evaluate(dict.testable, body(a));
            point(argument(a, ans))
        )

    /// QuickChecks `==>`
    pub def implies(testable: a -> Property, b: Bool, x:a): Property = match b {
        case true => testable(x)
        case false => testableUnit()
    }

    pub def label(testable: a -> Property, s: String, x:a): Property =
        let add = ans -> {
            let ss = ans.stamp;
            { stamp = s :: ss | ans }
        };
        Property(add `map` evaluate(testable, x))

    pub def classify(testable: a -> Property, b: Bool, name: String, x:a): Property = match b {
        case true => label(testable, name, x)
        case false => testable(x)
    }

    pub def trivial(testable: a -> Property, b: Bool, x: a): Property =
        classify(testable, b, "trivial", x)

    pub def collect(dict: {show :: a -> String, testable :: b -> Property}, v: a, x: b): Property =
        let show1 = dict.show;
        label(dict.testable, show1(v), x)


    type alias Config =
        { configMaxTest :: Int32
        , configMaxFail :: Int32
        , configSize :: Int32 -> Int32
        , configEvery :: (Int32, List[String]) -> String
        }

    pub def quick(): Config =
        { configMaxTest = 100
        , configMaxFail= 1000
        , configSize = x -> x / 2 + 3
        , configEvery = (_,_) -> ""
        }

    pub def verbose(): Config =
        let every = (n,args) -> { let x = Int32.toString(n) + ":"; String.unlines(x :: args)} ;
        { configEvery = every | quick() }


    pub def quickCheck(testable: a -> Property, x: a): Unit & Impure =
        check(testable, quick(), x)

    pub def verboseCheck(testable: a -> Property, x: a): Unit & Impure =
        check(testable, verbose(), x)

    pub def check(testable: a -> Property, config: Config, x: a): Unit & Impure =
        let rnd = newRandom();
        let ans = evaluate(testable, x);
        tests(config, ans, rnd, 0, 0, Nil)

    def tests(config: Config,  gen: Gen[Answer], rnd0: SplittableRandom,  ntest: Int32, nfail: Int32,  stamps: List[List[String]]): Unit & Impure =
        match (config.configMaxTest, config.configMaxFail) {
            case (maxtest, _) if maxtest == ntest => done("Ok, passed", ntest, stamps)
            case (_, maxfail) if maxfail == nfail => done("Arguments exhausted after", ntest, stamps)
            case _ => {
                let (rnd1,rnd2) = split2(rnd0);
                let getConfigSize = config.configSize;
                let answer = generate(getConfigSize(ntest), rnd2, gen);
                printEvery(config.configEvery, ntest, answer.arguments);
                match answer.okay {
                    case None => tests(config, gen, rnd1, ntest, nfail + 1, stamps)
                    case Some(true) => tests(config, gen, rnd1, ntest + 1, nfail, (answer.stamp :: stamps))
                    case Some(false) => {
                        println("Falsifiable after " + Int32.toString(ntest) + " tests:");
                        List.foreach(println, answer.arguments)
                    }
                }
            }
        }

    def printEvery(showEvery: (Int32, List[String]) -> String, n: Int32, args: List[String]): Unit & Impure=
        match showEvery(n, args) {
            case "" => () as & Impure
            case str => println(str)
        }

    def done(msg: String, ntest: Int32, stamps: List[List[String]]): Unit & Impure =
        let s = msg + " " + Int32.toString(ntest) + " tests" + makeTable(ntest, stamps);
        println(s)

    def makeTable(ntest: Int32, stamps: List[List[String]]): String =
        let percentage = (n,m) -> { let x = (100 *n) / m; Int32.toString(x) + "%"};
        let entry = xy -> percentage(fst(xy), ntest) + " " + String.intercalate(", ", snd(xy));
        stamps
            |> List.filter(List.isEmpty)
            |> List.sortWith(listCompareWith(stringCompare))
            |> List.groupBy((s,t) -> s == t)
            |> List.map(pairLength)
            |> List.sortWith((u,v) -> Order.compare(fst(u), fst(v)))
            |> List.reverse
            |> List.map(entry)
            |> display


    def display(xs: List[String]): String = match xs {
        case Nil        => "." + String.lineSeparator()
        case x :: Nil   => "(" + x + ")." + String.lineSeparator()
        case _          => String.intercalate(String.lineSeparator(), "." :: List.map(s -> s + ".", xs))
    }

    def pairLength(xss: List[List[String]]): (Int32, List[String]) = match xss {
        case xs :: _ => (List.length(xss), xs)
        case Nil => (0, Nil)
    }


}

*/