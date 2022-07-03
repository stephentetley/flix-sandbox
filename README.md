# flix-sandbox

Incubator project for candidate library modules for Flix.

Note November 2021 - the regex modules have been moved into a
separate repo so they can be used without the dependencies `flix-sandbox`
needs. The new repo is:

https://github.com/stephentetley/flix-regex

March 2022 - the project layout has changed to work with Flix's 
package manager so it should be easier to build and use.

July 2022 - deprecated Marshal, removed Charset and Locale. These are now 
provided by separate packages.

Dependencies:

https://github.com/stephentetley/collection-interop
https://github.com/stephentetley/charset-locale

Apache Commons IO - currrently `commons-io-2.11.0.jar`
