
namespace BigMapAccum {

    def makeList(size: Int32): List[Int32] = 
        List.repeat(size, 1)


    @test
    def test01(): Unit & Impure =  
        let xs = makeList(2000000);
        match Traversable.traverse(x -> Some(x+1), xs) {
            case Some(xs1) => println("length(xs1): ${List.length(xs1)}")
            case None => println("None")
        };    
        let (sy, ys) = Traversable.mapAccumLeft((s, x) -> (s+1, x+s), 0, xs);
        println("state: ${sy}, length(ys): ${List.length(ys)}, head(ys): ${List.head(ys)}");

        let (sz, zs) = Traversable.mapAccumRight((s, x) -> (s+1, x+s), 0, xs);
        println("state: ${sz}, length(zs): ${List.length(zs)}, head(zs): ${List.head(zs)}");

        ()

}    