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


namespace FlixSandbox/System {



    /// pub opaque type  = ##java.lang.ProcessBuilder


    ///
    /// State and Codensity(Error) monad
    /// 
    /// `ka` is the continuation answer type
    ///
    pub opaque type ProcessBuilder[ka, a] = 
            ##java.lang.ProcessBuilder -> 
                (a -> ##java.lang.ProcessBuilder -> Result[ka, String] & Impure) -> Result[ka, String] & Impure


    instance Functor[ProcessBuilder[ka]] {
        pub def map(f: a -> b & ef, ma: ProcessBuilder[ka, a]): ProcessBuilder[ka, b] & ef = 
            FlixSandbox/System/ProcessBuilder.map(f, ma)
    }


    instance Applicative[ProcessBuilder[ka]] {
        pub def point(x: a): ProcessBuilder[ka, a] =  FlixSandbox/System/ProcessBuilder.point(x)
        pub def ap(mf: ProcessBuilder[ka, a -> b & ef], ma: ProcessBuilder[ka, a]): ProcessBuilder[ka, b] & ef =
            FlixSandbox/System/ProcessBuilder.ap(mf, ma)
    }

    instance Monad[ProcessBuilder[ka]] {
        pub def flatMap(f: a -> ProcessBuilder[ka, b] & ef, ma: ProcessBuilder[ka, a]): ProcessBuilder[ka, b] & ef =
            FlixSandbox/System/ProcessBuilder.flatMap(f, ma)
    }

}


namespace FlixSandbox/System/ProcessBuilder {

    use FlixSandbox/System.ProcessBuilder;
    use FlixSandbox/System.ProcessBuilder.{ProcessBuilder};

    use FlixSandbox/System.FilePath;
    use FlixSandbox/System.FilePath.{FilePath};
    use FlixSandbox/System.Process;
    use FlixSandbox/System.Process.{Process};

    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: ProcessBuilder[ka, a], 
                st: ##java.lang.ProcessBuilder, 
                k: a -> ##java.lang.ProcessBuilder -> Result[ka, String] & Impure): Result[ka, String] & Impure =
        let ProcessBuilder(ma1) = ma;
        ma1(st, k)



    pub def start(ma: ProcessBuilder[(##java.lang.ProcessBuilder, a), a]): Result[Process, String] & Impure = 
        import new java.lang.ProcessBuilder(##java.util.List): ##java.lang.ProcessBuilder & Impure as newProcessBuilder;
        import java.lang.ProcessBuilder.start(): ##java.lang.Process & Impure as start1;
        try {
            let strs = FlixSandbox/Marshal.marshalList(x1 -> x1 as ##java.lang.Object, Nil);
            let jbuilder = newProcessBuilder(strs);
            match runBuilder(ma, jbuilder) {
                case Err(msg)           => Err(msg)
                case Ok((jbuilder1, _)) => Ok(Process(start1(jbuilder1)))
            }
        } catch {
            case ex: ##java.lang.Exception => Err(FlixSandbox/Marshal.unmarshalExceptionMessage(ex))
        }


    ///
    /// Returns the result (new state and answer) of applying `ma` to the initial state `st`.
    ///
    def runBuilder(ma: ProcessBuilder[(##java.lang.ProcessBuilder, a), a], 
                    st: ##java.lang.ProcessBuilder): Result[(##java.lang.ProcessBuilder, a), String] & Impure =
        let ProcessBuilder(f) = ma;
        let cont = (a, s) -> Ok(s, a) as & Impure;
        f(st, cont)


    pub def point(x: a): ProcessBuilder[ka, a] =
        ProcessBuilder((s, k) -> k(x, s))

    pub def map(f: a -> b & ef, ma: ProcessBuilder[ka, a]): ProcessBuilder[ka, b] & ef =
        ProcessBuilder((s, k) ->
            apply1(ma, s, (a, s1) -> k(f(a) as & Impure, s1))
        ) as & ef

    pub def ap(mf: ProcessBuilder[ka, a -> b & ef], ma: ProcessBuilder[ka, a]): ProcessBuilder[ka, b] & ef =
        ProcessBuilder((s, k) ->
            apply1(mf, s, (f, s1) ->
                apply1(ma, s1, (a, s2) ->
                    k(f(a) as & Impure, s2)))
        ) as & ef


    pub def flatMap(f: a -> ProcessBuilder[ka, b] & ef, ma: ProcessBuilder[ka, a]): ProcessBuilder[ka, b] & ef =
        ProcessBuilder((s, k) ->
            apply1(ma, s, (a, s1) ->
                apply1(f(a) as & Impure, s1, (b, s2) ->
                    k(b, s2)))
        ) as & ef

    pub def throwError(msg: String): ProcessBuilder[ka, a] = 
        ProcessBuilder((_, _) -> Err(msg) as & Impure)


    def liftJavaBuilderGetter(f: ##java.lang.ProcessBuilder-> a & Impure): ProcessBuilder[ka, a] = 
        ProcessBuilder((s, k) ->
            try {
                let ans = f(s);
                k(ans, s)
            } catch {
                case ex: ##java.lang.Exception => Err(FlixSandbox/Marshal.unmarshalExceptionMessage(ex))
            }            
        )

    def liftJavaBuilderSetter(f: ##java.lang.ProcessBuilder-> ##java.lang.ProcessBuilder & Impure): ProcessBuilder[ka, Unit] = 
        ProcessBuilder((s, k) ->
            try {
                let s1 = f(s);
                k((), s1)
            } catch {
                case ex: ##java.lang.Exception => Err(FlixSandbox/Marshal.unmarshalExceptionMessage(ex))
            }            
        )

    /// API...


    pub def command(cmd: String, args: List[String]): ProcessBuilder[ka, Unit] =
        import java.lang.ProcessBuilder.command(##java.util.List): ##java.lang.ProcessBuilder & Impure;
        let strs = FlixSandbox/Marshal.marshalList(x1 -> x1 as ##java.lang.Object, cmd :: args);
        liftJavaBuilderSetter(flip(command, strs))


    pub def setDirectory(p: FilePath): ProcessBuilder[ka, Unit] = 
        import java.lang.ProcessBuilder.directory(##java.io.File): ##java.lang.ProcessBuilder & Impure;
        import new java.io.File(String): ##java.io.File & Impure as newFile;
        let fn = pb -> {
            let filename = ToString.toString(p);
            let file1 = newFile(filename);
            directory(pb, file1)
        };
        liftJavaBuilderSetter(fn)

    pub def getDirectory(): ProcessBuilder[ka, FilePath] = 
        import java.lang.ProcessBuilder.directory(): ##java.io.File & Impure;
        import java.io.File.toPath(): ##java.nio.file.Path & Impure;
        liftJavaBuilderGetter(pb -> directory(pb) |> toPath |> FilePath)

}
