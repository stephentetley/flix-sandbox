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


namespace FlixSandbox/System {


    ///
    /// Reader (without local) and Error monad in codensity style to be stack safe.
    /// 
    /// `ka` is the continuation answer type
    ///
    pub enum ProcessBuilder[r: Region, ka: Type, a: Type](
        ##java.lang.ProcessBuilder -> (a -> Result[ka, String] & r) -> Result[ka, String] & r)


    instance Functor[ProcessBuilder[r, ka]] {
        pub def map(f: a -> b & ef, ma: ProcessBuilder[r, ka, a]): ProcessBuilder[r, ka, b] & ef = 
            FlixSandbox/System/ProcessBuilder.map(f, ma)
    }


    instance Applicative[ProcessBuilder[r, ka]] {
        pub def point(x: a): ProcessBuilder[r, ka, a] =  FlixSandbox/System/ProcessBuilder.point(x)
        pub def ap(mf: ProcessBuilder[r, ka, a -> b & ef], ma: ProcessBuilder[r, ka, a]): ProcessBuilder[r, ka, b] & ef =
            FlixSandbox/System/ProcessBuilder.ap(mf, ma)
    }

    instance Monad[ProcessBuilder[r, ka]] {
        pub def flatMap(f: a -> ProcessBuilder[r, ka, b] & ef, ma: ProcessBuilder[r, ka, a]): ProcessBuilder[r, ka, b] & ef =
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
    def apply1(ma: ProcessBuilder[r, ka, a], 
                s: ##java.lang.ProcessBuilder, 
                k: a -> Result[ka, String] & r): Result[ka, String] & r =
        let ProcessBuilder(ma1) = ma;
        ma1(s, k)


    ///
    /// Returns the result of applying `ma` to the initial state `s`.
    ///
    def runBuilder(_: Region[r], ma: ProcessBuilder[r, a, a], 
                    s: ##java.lang.ProcessBuilder): Result[a, String] & r =
        let ProcessBuilder(f) = ma;
        let cont = a -> Ok(a) as & r;
        f(s, cont)


    pub def point(x: a): ProcessBuilder[r, ka, a] =
        ProcessBuilder((_, k) -> k(x))

    pub def map(f: a -> b & ef, ma: ProcessBuilder[r, ka, a]): ProcessBuilder[r, ka, b] & ef =
        ProcessBuilder((s, k) ->
            apply1(ma, s, a -> k(f(a) as & r))
        ) as & ef

    pub def ap(mf: ProcessBuilder[r, ka, a -> b & ef], ma: ProcessBuilder[r, ka, a]): ProcessBuilder[r, ka, b] & ef =
        ProcessBuilder((s, k) ->
            apply1(mf, s, f ->
                apply1(ma, s, a ->
                    k(f(a) as & r)))
        ) as & ef


    pub def flatMap(f: a -> ProcessBuilder[r, ka, b] & ef, ma: ProcessBuilder[r, ka, a]): ProcessBuilder[r, ka, b] & ef =
        ProcessBuilder((s, k) ->
            apply1(ma, s, a ->
                apply1(f(a) as & r, s, b ->
                    k(b)))
        ) as & ef

    pub def throwError(msg: String): ProcessBuilder[r, ka, a] = 
        ProcessBuilder((_, _) -> Err(msg) as & r)

    /// Must be Impure uses IO...
    pub def start(r: Region[r], ma: ProcessBuilder[r, a, a]): Result[Process, String] \ { Write(r), Impure } = 
        import new java.lang.ProcessBuilder(##java.util.List): ##java.lang.ProcessBuilder & r as newProcessBuilder;
        import java.lang.ProcessBuilder.start(): ##java.lang.Process & Impure as start1;
        try {
            let strs = FlixSandbox/Marshal.marshalList(x1 -> x1 as ##java.lang.Object, Nil);
            let jbuilder = newProcessBuilder(strs);
            match runBuilder(r, ma, jbuilder) {
                case Err(msg) => Err(msg)
                case Ok(_)    => Ok(Process(start1(jbuilder)))
            }
        } catch {
            case ex: ##java.lang.Exception => 
                import java.lang.Throwable.getMessage(): String & r;
                Err(getMessage(ex))
        }

    def liftJavaBuilderGetter(f: ##java.lang.ProcessBuilder-> a & ef): ProcessBuilder[r, ka, a] & ef = 
        ProcessBuilder((s, k) ->
            try {
                let ans = f(s) as & r;
                k(ans)
            } catch {
                case ex: ##java.lang.Exception => 
                    import java.lang.Throwable.getMessage(): String & r;
                    Err(getMessage(ex))
            }            
        ) as & ef

    def liftJavaBuilderSetter(f: ##java.lang.ProcessBuilder-> ##java.lang.ProcessBuilder & ef): ProcessBuilder[r, ka, Unit] & ef = 
        ProcessBuilder((s, k) ->
            try {
                { discard f(s); k()} as & r
            } catch {
                case ex: ##java.lang.Exception => 
                    import java.lang.Throwable.getMessage(): String & r;
                    Err(getMessage(ex))
            }            
        ) as & ef

    /// API...


    pub def command(cmd: String, args: List[String]): ProcessBuilder[r, ka, Unit] \ Write(r) =
        import java.lang.ProcessBuilder.command(##java.util.List): ##java.lang.ProcessBuilder & r;
        let strs = FlixSandbox/Marshal.marshalList(x1 -> x1 as ##java.lang.Object, cmd :: args);
        liftJavaBuilderSetter(flip(command, strs))


    pub def setDirectory(p: FilePath): ProcessBuilder[r, ka, Unit] \ Write(r) = 
        import java.lang.ProcessBuilder.directory(##java.io.File): ##java.lang.ProcessBuilder & r;
        import new java.io.File(String): ##java.io.File & r as newFile;
        let fn = pb -> {
            let filename = ToString.toString(p);
            let file1 = newFile(filename);
            directory(pb, file1)
        };
        liftJavaBuilderSetter(fn)

    pub def getDirectory(): ProcessBuilder[r, ka, FilePath] \ Write(r) = 
        import java.lang.ProcessBuilder.directory(): ##java.io.File & r;
        import java.io.File.toPath(): ##java.nio.file.Path & r;
        liftJavaBuilderGetter(pb -> directory(pb) |> toPath |> FilePath)

}
