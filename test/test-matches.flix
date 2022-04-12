

namespace TestMatches {

@test
def test01(): Unit & Impure = 
    println("TestMatches.test01...");
    let path = "E://coding//flix//flix-sandbox";
    match FlixSandbox/System/Extras.matches("*.*", path) {
        case Err(msg) => println("Err: ${msg}")
        case Ok(xs)   => List.foreach(println, xs)
    };
    ()

@test
def test02(): Unit & Impure = 
    println("TestMatches.test01...");
    let path = "E://coding//flix//flix-sandbox";
    match FlixSandbox/System/Extras.matches("*.md", path) {
        case Err(msg) => println("Err: ${msg}")
        case Ok(xs)   => List.foreach(println, xs)
    };
    ()

}
