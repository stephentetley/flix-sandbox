
def leaf(x: String): RoseTree[String] = RoseTree(x, Nil)

def node(x: String, xs: List[RoseTree[String]]): RoseTree[String] = RoseTree(x, xs)

@test
def drawTree01(): String = 
    let t1 = RoseTree("a", Nil);
    RoseTree.drawTree(t1)

@test
def drawTree02(): String = 
    let t1 = node("a", leaf("b") :: leaf("c") :: Nil);
    RoseTree.drawTree(t1)    


@test
def drawTree03(): String = 
    let t1 = node("a/1", node("b/2", leaf("d/3") :: leaf("e/3") :: Nil) :: leaf("c/2") :: Nil);
    RoseTree.drawTree(t1)        