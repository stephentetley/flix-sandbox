

def main(): Unit & Impure = 
    println("Running...");
    test01();
    test02();
    ()

def test01(): Unit & Impure = 
    let arr1 = [1, 2, 3, 4] @ Static;
    let iarr1 = Doodle/ImmArray.fromArray(arr1);
    println(iarr1)


def test02(): Unit & Impure = 
    let arr1 = [Some(1), None, Some(3), None] @ Static;
    let iarr1 = Doodle/ImmArray.fromArray(arr1);
    println(iarr1)    

