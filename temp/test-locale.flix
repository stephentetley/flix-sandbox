

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running ${Time/LocalTime.now()}...");
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
    I18n/Locale.getIsoCountries() 
        |> List.foreach(println) 

def test02(): Unit & Impure = 
    I18n/Locale.getIsoCountriesWithIsoCountryCode(I18n/Locale/IsoCountryCode.Part1Alpha2) 
        |> Set.foreach(println) 

def test03(): Unit & Impure = 
    I18n/Locale.getIsoLanguages()
        |> List.foreach(println) 

def test04(): Unit & Impure = 
    I18n/Locale.getAvailableLocales()
        |> Array.foreach(println) 
