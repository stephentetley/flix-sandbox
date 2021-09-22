

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running ${Time/LocalTime.now()}...");
    runTest("test01", _ -> test01());
    runTest("test02", _ -> test02());
    runTest("test03", _ -> test03());
    runTest("test03a", _ -> test03a());
    runTest("test04", _ -> test04());
    runTest("test05", _ -> test05());
    0


def runTest(msg: String, f: Unit ~> Result[a, String]): Unit & Impure with ToString[a]= 
    println(msg);
    f() |> println

def test01(): Result[Unit, String] & Impure = 
    use Result.{flatMap};
    let* patt = Text/Regex/Pattern.compile("[Oo]");
    Text/Regex.replaceFirstMatch(patt, "0", "Hello World") |> println;
    Ok()

def test02(): Result[Int32, String] & Impure =
    use Result.{flatMap};
    let* patt = Text/Regex/Pattern.compile("(\\p{Digit}+)");
    let eval1 = Text/Regex/MatchEvaluator.getInt32At(0);
    Text/Regex.evalMatches(patt, eval1, "1000")

def test03(): Result[Bool, String] & Impure =
    use Result.{flatMap};
    use Text/Regex/Matcher.{runMatcher, matches};
    let* patt = Text/Regex/Pattern.compile("(\\p{Digit}+)");
    runMatcher(matches(), patt, "1000")

/// Note - have to find first... This needs a big warning!
def test03a(): Result[Int32, String] & Impure =
    use Result.{flatMap};
    use Text/Regex/Matcher.{seqR, evaluate, find, runMatcher};
    let* patt = Text/Regex/Pattern.compile("(\\p{Digit}+)");
    let eval1 = Text/Regex/MatchEvaluator.getInt32At(0);
    runMatcher(seqR(find(), evaluate(eval1)), patt, "1000")

def test04(): Result[Int32, String] & Impure =
    use Result.{flatMap};
    use Text/Regex/Matcher.{seqR, evaluate, find, runMatcher};
    let* patt = Text/Regex/Pattern.compile("Set\\p{Blank}*(?<set>\\p{Digit}+)");
    let eval1 = Text/Regex/MatchEvaluator.getInt32Named("set");
    runMatcher(seqR(find(), evaluate(eval1)), patt, "Set 1 Match 1")    


def test05(): Result[Unit, String] & Impure =
    use Result.{flatMap};
    let* patt = Text/Regex/Pattern.compile("\\p{Blank}+");
    Text/Regex.indexOfLeft(patt, "Set 1 Match 1") |> println; 
    Text/Regex.indexOfRight(patt, "Set 1 Match 1") |> println;    
    String.indexOfRight(" ", "Set 1 Match 1") |> println;
    Ok()
