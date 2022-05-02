

def main(): Unit & Impure = 
    println("Running...");
    println("test01...");
    test01();
    println("test02...");
    test02();
    println("test03...");
    test03();
    println("test04...");
    test04();
    ()

def test01(): Unit & Impure = 
    match I18n/Locale.getIsoCountries() {
        case Ok(l) => List.foreach(println, l) 
        case Err(msg) => println("Err: ${msg}")
    }

def test02(): Unit & Impure = 
    match I18n/Locale.getIsoCountriesWithIsoCountryCode(I18n/Locale/IsoCountryCode.Part1Alpha2) {
        case Ok(s) => Set.foreach(println, s) 
        case Err(msg) => println("Err: ${msg}")
    }

def test03(): Unit & Impure = 
    match I18n/Locale.getIsoLanguages() {
        case Ok(l) => List.foreach(println, l) 
        case Err(msg) => println("Err: ${msg}")
    }

def test04(): Unit & Impure = 
    I18n/Locale.getAvailableLocales()
        |> Array.foreach(println) 
