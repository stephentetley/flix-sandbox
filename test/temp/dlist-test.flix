

@test
def empty01(): Bool = 
    DList.empty() |> DList.toList |> (x -> x == Nil)


@test
def singleton01(): Bool = 
    DList.singleton(1) |> DList.toList |> (x -> x == 1 :: Nil)

@test
def append01(): Bool = 
    let xs = DList.append(DList.singleton(1), DList.singleton(2));
    xs |> DList.toList |> (x -> x == 1 :: 2 :: Nil)

@test
def cons01(): Bool = 
    let xs = DList.cons(1, DList.empty());
    xs |> DList.toList |> (x -> x == 1 :: Nil)

@test
def cons02(): Bool = 
    let xs = DList.cons(1, DList.singleton(2));
    xs |> DList.toList |> (x -> x == 1 :: 2 :: Nil)

@test
def snoc01(): Bool = 
    let xs = DList.snoc(DList.empty(), 2);
    xs |> DList.toList |> (x -> x == 2 :: Nil)

@test
def snoc02(): Bool = 
    let xs = DList.snoc(DList.singleton(1), 2);
    xs |> DList.toList |> (x -> x == 1 :: 2 :: Nil)
