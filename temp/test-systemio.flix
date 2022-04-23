

def main(): Unit & Impure = 
    println("Running... ${Time/LocalTime.now()}");
    test01() |> println;
    foldablePrintBytesWith([48i8, 49i8, 50i8]) |> println;
    test02() |> println;
    test03() |> println;
    test04() |> println;
    test05() |> println;
    ()

def test01(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let path1   = System/FilePath.new("E://coding//flix//flix-sandbox//output1.bin");
    let* _      = System/IO.writeBytes(path1, [48i8, 49i8, 50i8]);
    let* arr1   = System/IO.readBytes(path1);
    Array.foreach(println, arr1);
    Ok()

def foldablePrintBytesWith[t: Type -> Type](bytes: t[Int8]): Result[Unit, String] & Impure with Foldable[t] =
    Foldable.foldLeft((_, x) -> println(x), (), bytes);
    Ok()

def test02(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let path1   = System/FilePath.new("E://coding//flix//flix-sandbox");
    let iter    = System/Directory.contentsIter(path1);
    let xs      = Iterator.toList(iter);
    List.foreach(println, xs);
    Ok()


def test03(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let path1   = System/FilePath.new("E://coding//flix//flix-sandbox");
    let* xs     = System/Directory.contents(path1);
    List.foreach(println, xs);
    Ok() 

def test04(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let path1   = System/FilePath.new("E://coding//flix//flix-sandbox//src//flix//System");
    let* xs     = System/Directory.matches("*.flix", path1);
    List.foreach(println, xs);
    Ok() 

def test05(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let path1   = System/FilePath.new("E://coding//flix//flix-sandbox//src//java");
    let* xs     = System/Directory.contentsAll(false, path1);
    List.foreach(println, xs);
    Ok() 