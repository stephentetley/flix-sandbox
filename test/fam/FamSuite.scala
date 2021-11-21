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

package spt.fam

import ca.uwaterloo.flix.util.{FlixTest, Options}
import org.scalatest.Suites

class FamSuite extends Suites(
  new FlixTest("TestApplicative", "main/test/spt/fam/TestApplicative.flix")(Options.TestWithLibAll),
  new FlixTest("TestAlternative", "main/test/spt/fam/TestAlternative.flix")(Options.TestWithLibAll),
  new FlixTest("TestFunctor", "main/test/spt/fam/TestFunctor.flix")(Options.TestWithLibAll),
  new FlixTest("TestIdentity", "main/test/spt/fam/TestIdentity.flix")(Options.TestWithLibAll),
  new FlixTest("TestMonad", "main/test/spt/fam/TestMonad.flix")(Options.TestWithLibAll),
)
