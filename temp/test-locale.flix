

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running...");
    Console.printLine("test01...");
    test01();
    Console.printLine("test02...");
    test02();
    Console.printLine("test03...");
    test03();
    Console.printLine("test04...");
    test04();
    0

def test01(): Unit & Impure = 
    match I18n/Locale.getIsoCountries() {
        case Ok(l) => List.foreach(println, l) 
        case Err(msg) => Console.printLine("Err: ${msg}")
    }

def test02(): Unit & Impure = 
    match I18n/Locale.getIsoCountriesWithIsoCountryCode(I18n/Locale/IsoCountryCode.Part1Alpha2) {
        case Ok(s) => Set.foreach(println, s) 
        case Err(msg) => Console.printLine("Err: ${msg}")
    }

def test03(): Unit & Impure = 
    match I18n/Locale.getIsoLanguages() {
        case Ok(l) => List.foreach(println, l) 
        case Err(msg) => Console.printLine("Err: ${msg}")
    }

def test04(): Unit & Impure = 
    I18n/Locale.getAvailableLocales()
        |> Array.foreach(println) 
