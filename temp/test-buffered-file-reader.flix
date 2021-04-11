
/// This indicates Cursor working correctly, but we have a problem in sheetio...

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running ${Time/LocalTime.now()}...");
    Console.printLine("test01... head!");
    test01();
    Console.printLine("test02... drop!");
    test02();
    0

def test01(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let src = System/FilePath.new("E:/coding/flix/flix-sandbox/data/numbers.txt");
    let* reader = System/BufferedFileReader.new(src, Text/Charset.utf_8());
    let cursor = System/BufferedFileReader.lines(reader);
    let a1 = Cursor.head!(cursor);
    Console.printLine("head! was ${a1}");
    let a2 = Cursor.head!(cursor);
    Console.printLine("head! was ${a2}");
    let rs = Cursor.toList!(cursor);
    Console.printLine("rest of input was ${rs}");
    Ok()

def test02(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let src = System/FilePath.new("E:/coding/flix/flix-sandbox/data/numbers.txt");
    let* reader = System/BufferedFileReader.new(src, Text/Charset.utf_8());
    let cursor = System/BufferedFileReader.lines(reader);
    Cursor.drop!(2, cursor);
    let rs = Cursor.toList!(cursor);
    Console.printLine("rest of input was ${rs}");
    Ok()    
