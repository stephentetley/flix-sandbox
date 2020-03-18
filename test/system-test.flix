

@test
def new01(): FilePath & Impure = 
    FilePath.new("e:/coding/flix")

@test
def new02(): FilePath & Impure = 
    FilePath.new("..")

@test
def toString01(): String & Impure = 
    FilePath.new("e:/coding/flix") |> FilePath.toString

@test
def toString02(): String & Impure = 
    FilePath.new("..") |> FilePath.toString


@test
def normalize01(): String & Impure = 
    FilePath.new("c:/root/child1/..") |> FilePath.normalize |> FilePath.toString

@test
def getCurrentDirectory01(): FilePath & Impure = 
    Directory.getCurrentDirectory()

@test
def readFile01(): String & Impure = 
    let path = FilePath.new("E:\coding\flix\flix-system\data\colours.txt");
    match File.readFile(path, Charset.utf_8()) {
        case Err(msg) => "***" + msg + "***"
        case Ok(s) => s
    }

@test
def writeFile01(): String & Impure = 
    let path = FilePath.new("E:\coding\flix\flix-system\data\colours-out.txt");
    match File.writeFile(path, Charset.utf_8(), "red, green, blue") {
        case Err(msg) => "***" + msg + "***"
        case Ok(_) => "Okay"
    }