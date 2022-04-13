

namespace TestMatches {

@test
def test01(): Unit & Impure = 
    println("TestMatches.test01...");
    let path = "E://coding//flix//flix-sandbox";
    match FlixSandbox/System/Extras.globList(glob = "*.*", path) {
        case Err(msg) => println("Err: ${msg}")
        case Ok(xs)   => List.foreach(println, xs)
    };
    ()

@test
def test02(): Unit & Impure = 
    println("TestMatches.test02...");
    let path = "E://coding//flix//flix-sandbox";
    match FlixSandbox/System/Extras.globList(glob = "*.md", path) {
        case Err(msg) => println("Err: ${msg}")
        case Ok(xs)   => List.foreach(println, xs)
    };
    ()

@test
def test03(): Unit & Impure = 
    println("TestMatches.test03...");
    let path = "E://coding//flix//flix-sandbox";
    match FlixSandbox/System/Extras.regexList(regex = ".+\\.[Mm][Dd]", path) {
        case Err(msg) => println("Err: ${msg}")
        case Ok(xs)   => List.foreach(println, xs)
    };
    ()
}
