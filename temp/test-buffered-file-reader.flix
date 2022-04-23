
/// This indicates Cursor working correctly, but we have a problem in sheetio...

def main(): Unit & Impure = 
    println("Running ${Time/LocalTime.now()}...");
    println("test01... head!");
    test01();
    println("test02... drop!");
    test02();
    ()

def test01(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let src = System/FilePath.new("E:/coding/flix/flix-sandbox/data/numbers.txt");
    let* reader = System/BufferedFileReader.new(src, Text/Charset.utf_8());
    let cursor = System/BufferedFileReader.lines(reader);
    let a1 = Cursor.head!(cursor);
    println("head! was ${a1}");
    let a2 = Cursor.head!(cursor);
    println("head! was ${a2}");
    let rs = Cursor.toList!(cursor);
    println("rest of input was ${rs}");
    Ok()

def test02(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let src = System/FilePath.new("E:/coding/flix/flix-sandbox/data/numbers.txt");
    let* reader = System/BufferedFileReader.new(src, Text/Charset.utf_8());
    let cursor = System/BufferedFileReader.lines(reader);
    Cursor.drop!(2, cursor);
    let rs = Cursor.toList!(cursor);
    println("rest of input was ${rs}");
    Ok()    
