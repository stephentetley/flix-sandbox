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

namespace TestPattern {


/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def toString01(): Bool =
    use Result.flatMap;
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\s+");
        Ok(Text/Regex/Pattern.toString(p1))
    };
    step() |> Result.isOk

/////////////////////////////////////////////////////////////////////////////
// getPattern                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def getPattern01(): Bool =
    use Result.flatMap;
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\s+");
        Ok(Text/Regex/Pattern.getPattern(p1))
    };
    step() == Ok("\s+")

/////////////////////////////////////////////////////////////////////////////
// getFlags                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def getFlags01(): Bool =
    use Result.flatMap;
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\s+");
        Ok(Text/Regex/Pattern.getFlags(p1))
    };
    step() == Ok(Nil)

@test
def getFlags02(): Bool =
    use Result.flatMap;
    use Text/Regex/Pattern.Flag.{CanonEq};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compileWithFlags("\s+", CanonEq :: Nil);
        Ok(Text/Regex/Pattern.getFlags(p1))
    };
    step() == Ok(CanonEq :: Nil)

@test
def getFlags03(): Bool =
    use Result.flatMap;
    use Text/Regex/Pattern.Flag.{CanonEq, Multiline};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compileWithFlags("\s+", CanonEq ::  Multiline :: Nil);
        Ok(Text/Regex/Pattern.getFlags(p1))
    };
    step() == Ok(CanonEq :: Multiline :: Nil) || step() == Ok(Multiline :: CanonEq :: Nil)

}