


def makeList(size: Int32): List[Int32] = 
    List.repeat(size, 1)

def main(_args: Array[String]): Int32 & Impure = 
    println("Running...");
    test01();
    0

def test01(): Unit & Impure =  
    let xs = makeList(2000000);
    match Traversable.traverse(x -> Some(x+1), xs) {
        case Some(xs1) => println("length(xs1)= ${List.length(xs1)}")
        case None => println("None")
    };    
    let (st, ys) = Traversable.mapAccumLeft((s, x) -> (s+1, x+s), 0, xs);
    println("state: ${st}, length(ys)= ${List.length(ys)}");
    ()