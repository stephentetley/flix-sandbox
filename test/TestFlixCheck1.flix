use Test/FlixCheck.{quickCheck, testableFunction, testableBool, genInt32};

def prop_RevUnit(x: Int32): Bool = List.reverse(x :: Nil) == x :: Nil

@test
def test01(): Unit & Impure = 
    let testable = testableFunction({show = Int32.toString, testable = testableBool, arbitrary = genInt32()});
    quickCheck(testable, prop_RevUnit)


// pub def bounds(n: Int32): Unit & Impure = 
//     Console.printLine(Int32.toString(-n) + "," + Int32.toString(n))