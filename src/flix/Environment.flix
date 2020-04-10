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


namespace System/Environment {

    /// Get env var...
    pub def getEnv(name: String): Result[String, String] & Impure =
        try {
            import java.lang.System:getenv(String);
            let ans = getenv(name);
            if (isNull(ans))
                Err("Not found")
            else
                Ok(ans)
        } catch {
            case _:##java.lang.Exception => Err("System exception")
        }



}