

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running... ${Time/LocalTime.now()}");
    test01() |> println;
    foldablePrintBytesWith([48i8, 49i8, 50i8]) |> println;
    test02() |> println;
    test03() |> println;
    test04() |> println;
    0

def test01(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let path1   = System/FilePath.new("E://coding//flix//flix-sandbox//output1.bin");
    let* _      = IOPrelude.writeBytes(path1, [48i8, 49i8, 50i8]);
    let* arr1   = IOPrelude.readBytes(path1);
    Array.foreach(println, arr1);
    Ok()

def foldablePrintBytesWith[t: Type -> Type](bytes: t[Int8]): Result[Unit, String] & Impure with Foldable[t] =
    Foldable.foldLeft((_, x) -> println(x), (), bytes);
    Ok()

def test02(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let path1   = System/FilePath.new("E://coding//flix//flix-sandbox");
    let iter    = IOPrelude.contentsIter(path1);
    let xs      = Iterator.toList(iter);
    List.foreach(println, xs);
    Ok()


def test03(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let path1   = System/FilePath.new("E://coding//flix//flix-sandbox");
    let* xs     = IOPrelude.contents(path1);
    List.foreach(println, xs);
    Ok() 

def test04(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let path1   = System/FilePath.new("E://coding//flix//flix-sandbox//src//flix//System");
    let* xs     = IOPrelude.matches("*.flix", path1);
    List.foreach(println, xs);
    Ok() 