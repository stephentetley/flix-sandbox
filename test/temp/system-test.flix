

@test
def new01(): System.FilePath & Impure = 
    System/FilePath.new("e:/coding/flix")

@test
def new02(): System.FilePath & Impure = 
    System/FilePath.new("..")

@test
def toString01(): String & Impure = 
    System/FilePath.new("e:/coding/flix") |> System/FilePath.toString

@test
def toString02(): String & Impure = 
    System/FilePath.new("..") |> System/FilePath.toString


@test
def normalize01(): String & Impure = 
    System/FilePath.new("c:/root/child1/..") |> System/FilePath.normalize |> System/FilePath.toString

@test
def getCurrentDirectory01(): System.FilePath & Impure = 
    System/Directory.getCurrentDirectory()

@test
def readFile01(): String & Impure = 
    let path = System/FilePath.new("E:\coding\flix\flix-system\data\colours.txt");
    match System/File.readFile(path, System/Charset.utf_8()) {
        case Err(msg) => "***" + msg + "***"
        case Ok(s) => s
    }

@test
def writeFile01(): String & Impure = 
    let path = System/FilePath.new("E:\coding\flix\flix-system\data\colours-out.txt");
    match System/File.writeFile(path, System/Charset.utf_8(), "red, green, blue") {
        case Err(msg) => "***" + msg + "***"
        case Ok(_) => "Okay"
    }

@test
def getEnvVars01(): Result[Unit, String] & Impure = 
    use Result.flatMap;
    let* m1 = System/Environment.getEnvVars();
    Map.foreach((k,v) -> { Console.printLine("Key: " + k); Console.printLine("Value:" + v)}, m1);
    Ok()

@test
def contents01(): Result[Unit, String] & Impure = 
    use Result.flatMap;
    let cwd = System/Directory.getCurrentDirectory();
    let* xs = System/Directory.contents(cwd);
    List.foreach(x -> Console.printLine(System/FilePath.toString(x)), xs);
    Ok()

@test
def contentsAll01(): Result[Unit, String] & Impure = 
    use Result.flatMap;
    let cwd = System/Directory.getCurrentDirectory();
    let* xs = System/Directory.contentsAll(cwd, false);
    List.foreach(x -> Console.printLine(System/FilePath.toString(x)), xs);
    Ok()
