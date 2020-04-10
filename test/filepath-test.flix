

@test
def normalize01(): String & Impure = 
    System/FilePath.new("c:/root/child1/..") |> System/FilePath.normalize |> System/FilePath.toString

@test
def isAbsolute01(): Bool & Impure = 
    System/FilePath.new("c:/root/child1/..") |> System/FilePath.isAbsolute

@test
def isRelative01(): Bool & Impure = 
    System/FilePath.new("../up1/test1.txt") |> System/FilePath.isRelative

@test
def getRoot01(): String & Impure = 
    match System/FilePath.new("c:/root/child1/file1.txt") |> System/FilePath.getRoot {
        case None => "**rel path, no root**"
        case Some(x) => System/FilePath.toString(x)
    }

@test
def getRoot02(): String & Impure = 
    match System/FilePath.new("../up1/file1.txt") |> System/FilePath.getRoot {
        case None => "**rel path, no root**"
        case Some(x) =>  System/FilePath.toString(x)
    }
        
@test
def getParent01(): String & Impure = 
    match System/FilePath.new("c:/root/child1/file1.txt") |> System/FilePath.getParent {
        case None => "**no parent**"
        case Some(x) => System/FilePath.toString(x)
    }

@test
def getParent02(): String & Impure = 
    match System/FilePath.new("file1.txt") |> System/FilePath.getParent {
        case None => "**no parent**"
        case Some(x) => System/FilePath.toString(x)
    }

@test
def getFileName01(): String & Impure = 
    match System/FilePath.new("file1.txt") |> System/FilePath.getFileName {
        case None => "**fail**"
        case Some(x) => System/FilePath.toString(x)
    }

@test
def getFileName02(): String & Impure = 
    match System/FilePath.new("g:") |> System/FilePath.getFileName {
        case None => "**fail**"
        case Some(x) => System/FilePath.toString(x)
    }

@test
def getFileName03(): String & Impure = 
    match System/FilePath.new("g:\directory1\directory2") |> System/FilePath.getFileName {
        case None => "**fail**"
        case Some(x) => System/FilePath.toString(x)
    }

@test
def append01(): String & Impure = 
    let p1 = System/FilePath.new("g:\directory1\directory2");
    let p2 = System/FilePath.new("file1.txt");
    System/FilePath.append(p1,p2) |> System/FilePath.toString
    
@test
def getNameCount01(): Int32 & Impure = 
    System/FilePath.new("g:\directory1\directory2\file1.txt") |> System/FilePath.getNameCount
    
@test
def getNameCount02(): Int32 & Impure = 
    System/FilePath.new("g:") |> System/FilePath.getNameCount

@test 
def getExtension01(): String & Impure = 
    match System/FilePath.new("g:\directory1\directory2\file1.txt") |> System/FilePath.getExtension {
        case None => "**fail**"
        case Some(x) => x
    }