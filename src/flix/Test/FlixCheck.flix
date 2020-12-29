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



namespace Test/FlixCheck {

    ///
    /// Represents a random number generator.
    ///
    pub opaque type Random = ##java.util.SplittableRandom

    def newRandom(): Random & Impure = 
        import new java.util.SplittableRandom() as newSplittableRandom;
        Random(newSplittableRandom())
        
    // def nextInt(r: Random, max: Int32): Int32 & Impure = 
    //     import java.util.SplittableRandom.nextInt(Int32);
    //     let Random(r1) = r;
    //     r1.nextInt(max)

    // def nextLong(r: Random, max: Int64): Int64 & Impure = 
    //     import java.util.SplittableRandom.nextLong(Int64);
    //     let Random(r1) = r;
    //     r1.nextLong(max)

    def split(r: Random): Random & Impure = 
        import java.util.SplittableRandom.split();
        let Random(r1) = r;
        Random(r1.split())

    def split2(r: Random): (Random, Random) & Impure = 
        let rx = r.split();
        let ry = rx.split();
        (rx, ry)



    def randomR(lo: Int32, hi: Int32, r: Random): Int32 & Impure = 
        import java.util.SplittableRandom.nextInt(Int32, Int32);
        let Random(r1) = r;
        r1.nextInt(lo, hi)

    def randomR64(lo: Int64, hi: Int64, r: Random): Int64 & Impure = 
        import java.util.SplittableRandom.nextLong(Int64, Int64);
        let Random(r1) = r;
        r1.nextLong(lo, hi)

    def stringCompare(s: String, t: String): Int32 = 
        import java.lang.String.compareTo(String);
        s.compareTo(t) as & Pure

    def listCompareWith(cmp: (a,a) -> Int32, xs: List[a], ys: List[a]): Int32 = 
        match (xs,ys) {
            case (Nil, Nil)             => 0
            case (Nil, _)               => -1
            case (_, Nil)               => 1
            case (x :: xs1, y :: ys1)   => { 
                let c = cmp(x,y);
                if (c == 0) 
                    listCompareWith(cmp, xs1, ys1)
                else
                    c
                }
            
        }


    pub opaque type Gen[a] = (Int32, Random) ~> a

    pub class Arbitrary[a] {
        pub def arbitrary(): Gen[a]
    }

    pub class Coarbitrary[a] {
        pub def coarbitrary(x: a, ma: Gen[b]): Gen[b]
    }

    instance Arbitrary[Bool] {
        def arbitrary(): Gen[Bool] =
            let* arr = liftAction(_ -> [true, false]);
            elements(arr)
    }

    instance Coarbitrary[Bool] {
        def coarbitrary(b: Bool, ma: Gen[b]): Gen[b] = variant(if (b) 0 else 1, ma)
    }

    pub def run(x: Gen[a], bounds: Int32): a & Impure = 
        let r1 = newRandom();
        apply1(x, bounds, r1)


    pub def return(x: a): Gen[a] = Gen((_,_) -> x as & Impure)

    def apply1(x: Gen[a], n: Int32, r: Random): a & Impure =
        let Gen(f) = x;
        f(n, r)

    pub def map(f: a -> b, x: Gen[a]): Gen[b] = 
        Gen((b,r) -> 
            let a = apply1(x, b, r);
            f(a)
        )


    pub def ap(mf: Gen[a -> b], ma: Gen[a]): Gen[b] =
        Gen((b,r) ->
            let f = apply1(mf, b, r);
            let a = apply1(ma, b, r);
            f(a)
        )

    pub def bind(x: Gen[a], k: a -> Gen[b]): Gen[b] = 
        Gen((b,r) -> 
            let a = apply1(x, b, r); 
            let y = k(a);
            apply1(y, b, r)
        )

    pub def flatMap(k: a -> Gen[b], x: Gen[a]): Gen[b] = bind(x, k)

    pub def liftAction(f: Unit -> a & Impure): Gen[a] = 
        Gen((_,_) -> f())


    pub def sequence(xs: List[Gen[a]]): Gen[List[a]] = sequenceHelper(xs, return)

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

    pub def rand(): Gen[Random] = 
        Gen((_,r) -> r as & Impure)

    ///
    /// /// Promote is difficult (impossible?) to type in presence of effects
    ///
    /// pub def promote(f: a ~> Gen[b]): Gen[a ~> b] =
    ///     Gen((n, r) ->
    ///         let f1: a ~> b = a -> {
    ///             let mf: Gen[b] = f(a);
    ///             let ans: b = apply1(mf, n, r);
    ///             ans
    ///             };
    ///         f1
    ///     )

    pub def generate(n: Int32, rnd: Random, gen: Gen[a]): a & Impure = 
        let size = randomR(0, Int32.abs(n), rnd);   /// TODO remove abs
        apply1(gen, size, rnd)


    pub def choose(lo:Int32, hi: Int32): Gen[Int32] = 
        let* r1 = rand();
        let* v = liftAction(_ -> randomR(lo, hi, r1));
        return(v)

    pub def variant(v: Int32, m: Gen[a]): Gen[a] =
        Gen((n,r) ->
            let Gen(mf) = m;
            mf(n, makeRands(r, v))
        )

    def makeRands(r0: Random, n: Int32): Random & Impure =
        if (n <= 0)
            r0
        else {
            let (r1, r2) = split2(r0);
            let n1 = n / 2;
            let s = n % 2;
            if (s == 0)
                makeRands(r1, n1)
            else
                makeRands(r2, n1)

        }


    def chooseInt64(lo:Int64, hi: Int64): Gen[Int64] = 
        let* r1 = rand();
        let* v = liftAction(_ -> randomR64(lo, hi, r1));
        return(v)

    pub def elements(arr: Array[a]): Gen[a] = 
        let hi = Array.length(arr);
        let* ix = choose(0, hi);
        let* v = liftAction(_ -> arr[ix]);
        return(v)

    pub def vector(arb: Gen[a], n: Int32): Gen[Array[a]] = 
        Gen((b,r) -> 
            let f = _ -> apply1(arb, b, r);
            Array.init(f, n)
        )

    pub def list(arb: Gen[a], n: Int32): Gen[List[a]] = 
        if (n <= 0)
            return(Nil)
        else 
            listHelper(arb, n)

    def listHelper(arb: Gen[a], n: Int32): Gen[List[a]] = 
        Gen((b,r) -> 
            let f = _ -> apply1(arb, b, r);
            let arr = Array.init(f, n);
            Array.toList(arr)
        )



    pub def oneof(arr: Array[Gen[a]]): Gen[a] = flatMap(x -> x, elements(arr)) 

    pub def genUnit(): Gen[Unit] = return()

    pub def genBool(): Gen[Bool] =
        let* arr = liftAction(_ -> [true, false]);
        elements(arr)

    pub def genInt32(): Gen[Int32] = 
        sized(n -> choose(-n, n+1))
        


    pub def genInt64(): Gen[Int64] = 
        sized(n -> { let x = Int32.toInt64(n); chooseInt64(-x, x + 1i64) })

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
        let* a = genInt32();
        let* b = genInt32();
        let* c = genInt32();        
        return(fractionF32(a,b,c))

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
        let* a = genInt32();
        let* b = genInt32();
        let* c = genInt32();        
        return(fractionF64(a,b,c))

    pub def genTuple2(gena: Gen[a], genb: Gen[b]): Gen[(a,b)] = 
        let* a = gena;
        let* b = genb;
        return((a,b))

    pub def genTuple3(gena: Gen[a], genb: Gen[b], genc: Gen[c]): Gen[(a,b,c)] = 
        let* a = gena;
        let* b = genb;
        let* c = genc;
        return((a,b,c))

    pub def genList(gena: Gen[a]): Gen[List[a]] = 
        sized(n -> bind(choose(0,n+1), x -> list(gena, x)))

    type alias Answer = { okay: Option[Bool], stamp: List[String], arguments: List[String] }

    def nothing(): Answer = { okay = None, stamp = Nil, arguments = Nil }

    pub opaque type Property = Gen[Answer]

    pub def answer(ans: Answer): Property = 
        let ma = return(ans); 
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

    pub def testableFunction(dict: {show: a -> String, testable: b -> Property, arbitrary: Gen[a]}, f: a -> b): Property = 
        let dminus = {show = dict.show, testable = dict.testable};
        forall(dminus, dict.arbitrary, f)

    pub def forall(dict: {show: a -> String, testable: b -> Property}, gen: Gen[a], body: a -> b): Property = 
        let argument = (a,ans) -> { 
            let show1 = dict.show; 
            let args = show1(a) :: ans.arguments;             
            { arguments = args | ans }
        };
        Property(
            let* a = gen;
            let* ans = evaluate(dict.testable, body(a));
            return(argument(a, ans))
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

    pub def collect(dict: {show: a -> String, testable: b -> Property}, v: a, x:b): Property = 
        let show1 = dict.show;
        label(dict.testable, show1(v), x)


    type alias Config = 
        { configMaxTest: Int32
        , configMaxFail: Int32
        , configSize: Int32 -> Int32
        , configEvery: (Int32, List[String]) -> String
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

    def tests(config: Config,  gen: Gen[Answer], rnd0: Random,  ntest: Int32, nfail: Int32,  stamps: List[List[String]]): Unit & Impure = 
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
                        Console.printLine("Falsifiable after " + Int32.toString(ntest) + " tests:");
                        List.foreach(Console.printLine, answer.arguments)
                    }
                }
            }
        }

    def printEvery(showEvery: (Int32, List[String]) -> String, n: Int32, args: List[String]): Unit & Impure= 
        match showEvery(n, args) {
            case "" => () as & Impure
            case str => Console.printLine(str)
        }

    def done(msg: String, ntest: Int, stamps: List[List[String]]): Unit & Impure = 
        let s = msg + " " + Int32.toString(ntest) + " tests" + makeTable(ntest, stamps);
        Console.printLine(s)
        
    def makeTable(ntest: Int32, stamps: List[List[String]]): String = 
        let percentage = (n,m) -> { let x = (100 *n) / m; Int32.toString(x) + "%"};
        let entry = xy -> percentage(fst(xy), ntest) + " " + String.intercalate(", ", snd(xy));
        stamps 
            |> List.filter(List.isEmpty)
            |> List.sortWith(listCompareWith(stringCompare))
            |> List.groupBy((s,t) -> s == t)
            |> List.map(pairLength)
            |> List.sortWith((u,v) -> Int32.compare(fst(u),fst(v)))
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