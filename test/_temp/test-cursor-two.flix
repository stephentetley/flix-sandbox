

def main(_args: Array[String]): Int64 & Impure = 
    Console.printLine("Running...");
    test01(); 
    test02();
    test03()



def test01(): Int32 & Impure = 
    let s1:Cursor2[String, Int32] = Cursor2.range(2, 10) |> Cursor2.cons(1);
    let sum = Cursor2.fold((ac, x) -> ac + x, 0 , s1);
    Console.printLine("sum: ${sum}");
    0



def test02(): Int32 & Impure = 
    let s1:Cursor2[String, Int32] = Cursor2.range(1, 10);
    let a1 = Cursor2.head(s1);
    Console.printLine("head: ${a1}");
    let a2 = Cursor2.head(s1);
    Console.printLine("head again: ${a2}");
    match Cursor2.toList(s1) { 
        case Ok(xs) => Console.printLine("toList: ${xs}")
        case Err(e) => Console.printLine("toList: ${e}")
    };    
    0

def nowStream(): Cursor2[String, Time.LocalTime] = 
    Cursor2.unfoldWithIter(_ -> Some(Time/LocalTime.now()))

def test03(): Int32 & Impure = 
    /// Note s1 is infinite
    let s1:Cursor2[String, Time.LocalTime] = nowStream();
    let a1 = Cursor2.head(s1);
    Console.printLine("head: ${a1}");
    let a2 = Cursor2.head(s1);
    Console.printLine("head again: ${a2}");
    match Cursor2.toList(Cursor2.take(5 , s1)) { 
        case Ok(xs) => Console.printLine("toList: ${xs}")
        case Err(e) => Console.printLine("toList: ${e}")
    };    
    0    