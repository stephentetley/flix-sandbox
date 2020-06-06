use Regex/Matcher.{runMatcherCompilePattern};

@test
def pattern01(): Result[Regex.Pattern, Regex.Error] = 
    Regex/Pattern.compile(".*")

@test
def pattern02(): String = 
    use Result.flatMap;
    let step = () -> {
        let* patt = Regex/Pattern.compile(".*");
        let s = Regex/Pattern.toString(patt);
        Ok(s)
    };
    Result.getWithDefault(step(), "_ERROR_")


@test
def pattern03(): String = 
    Regex/Pattern.quote("let*")



@test
def rewrite01(): Result[String, Regex.Error] & Impure = 
    use Regex/MatchEvaluator.{transformValue};    
    use Regex.{replaceWith};
    use Result.flatMap;
    let* patt = Regex/Pattern.compile("Z(?<num>\d+)");
    replaceWith(patt, transformValue(s -> "#" + s + "#"), "<!-- Z1 Z2 A3 Z4 -->")

@test
def rewrite02(): Result[String, Regex.Error] & Impure = 
    use Regex/MatchEvaluator.{transformValue};    
    use Regex.{replaceWithIndex};
    use Result.flatMap;
    let* patt = Regex/Pattern.compile("Z(?<num>\d+)");
    replaceWithIndex(patt, ix -> transformValue(s -> "(" + Int32.toString(ix) + ")#" + s + "#"), "<!-- Z1 Z2 A3 Z4 -->")