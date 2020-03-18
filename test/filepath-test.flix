@test
def normalize01(): String & Impure = 
    FilePath.new("c:/root/child1/..") |> FilePath.normalize |> FilePath.toString

@test
def isAbsolute01(): Bool & Impure = 
    FilePath.new("c:/root/child1/..") |> FilePath.isAbsolute

@test
def isRelative01(): Bool & Impure = 
    FilePath.new("../up1/test1.txt") |> FilePath.isRelative

@test
def getRoot01(): String & Impure = 
    match FilePath.new("c:/root/child1/file1.txt") |> FilePath.getRoot {
        case None => "**rel path, no root**"
        case Some(x) =>  FilePath.toString(x)
    }

@test
def getRoot02(): String & Impure = 
    match FilePath.new("../up1/file1.txt") |> FilePath.getRoot {
        case None => "**rel path, no root**"
        case Some(x) =>  FilePath.toString(x)
    }
        
@test
def getParent01(): String & Impure = 
    match FilePath.new("c:/root/child1/file1.txt") |> FilePath.getParent {
        case None => "**no parent**"
        case Some(x) =>  FilePath.toString(x)
    }

@test
def getParent02(): String & Impure = 
    match FilePath.new("file1.txt") |> FilePath.getParent {
        case None => "**no parent**"
        case Some(x) =>  FilePath.toString(x)
    }

@test
def getFileName01(): String & Impure = 
    match FilePath.new("file1.txt") |> FilePath.getFileName {
        case None => "**fail**"
        case Some(x) =>  FilePath.toString(x)
    }

@test
def getFileName02(): String & Impure = 
    match FilePath.new("g:") |> FilePath.getFileName {
        case None => "**fail**"
        case Some(x) =>  FilePath.toString(x)
    }

@test
def getFileName03(): String & Impure = 
    match FilePath.new("g:\directory1\directory2") |> FilePath.getFileName {
        case None => "**fail**"
        case Some(x) =>  FilePath.toString(x)
    }

@test
def append01(): String & Impure = 
    let p1 = FilePath.new("g:\directory1\directory2");
    let p2 = FilePath.new("file1.txt");
    FilePath.append(p1,p2) |> FilePath.toString
    
@test
def getNameCount01(): Int32 & Impure = 
    FilePath.new("g:\directory1\directory2\file1.txt") |> FilePath.getNameCount
    
@test
def getNameCount02(): Int32 & Impure = 
    FilePath.new("g:") |> FilePath.getNameCount

@test 
def getExtension01(): String & Impure = 
    match FilePath.new("g:\directory1\directory2\file1.txt") |> FilePath.getExtension {
        case None => "**fail**"
        case Some(x) => x
    }