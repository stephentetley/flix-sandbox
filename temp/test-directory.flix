

def main(): Unit & Impure = 
    println("Running...");
    println("test01...");
    test01();
    println("test02...");
    test02();
    ()

def test01(): Unit & Impure = 
    let path = System/FilePath.new("E://coding//flix//flix-sandbox");
    let src = System/Directory.contents(path);
    Cursor.foreach(println, src) 
        |> flip(Result.getWithDefault, ())


def test02(): Unit & Impure = 
    let path = System/FilePath.new("E://coding//flix//flix-sandbox");
    let src = System/Directory.contentsAll(path, false);
    Cursor.foreach(println, src) 
        |> flip(Result.getWithDefault, ())

