

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running... ${Time/LocalTime.now()}");
    test01() |> println;
    0

def test01(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let path1   = System/FilePath.new("E://coding//flix//flix-sandbox//output1.bin");
    let* _      = IOPrelude.writeBytes(path1, [48i8, 49i8, 50i8]);
    let* arr1   = IOPrelude.readBytes(path1);
    Array.foreach(println, arr1);
    Ok()
