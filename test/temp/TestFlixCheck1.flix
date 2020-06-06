use Test/FlixCheck.{quickCheck, testableFunction, testableBool, genInt32, genList};

def prop_RevUnit(x: Int32): Bool = List.reverse(x :: Nil) == x :: Nil

@test
def test01(): Unit & Impure = 
    let testable = testableFunction({show = Int32.toString, testable = testableBool, arbitrary = genInt32()});
    quickCheck(testable, prop_RevUnit)


def prop_RevBad(xs: List[Int32]): Bool = List.reverse(xs) == xs

@test
def test02(): Unit & Impure = 
    let showList = _ -> "<list>";
    let testable = testableFunction({show = showList, testable = testableBool, arbitrary = genList(genInt32())});
    quickCheck(testable, prop_RevBad)
