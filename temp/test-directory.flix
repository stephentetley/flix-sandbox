

def main(_args: Array[String]): Int32 & Impure = 
    println("Running ${Time/LocalTime.now()}...");
    println("test01...");
    test01();
    println("test02...");
    test02();
    0

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

