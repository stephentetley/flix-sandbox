

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running ${Time/LocalTime.now()}...");
    Console.printLine("test01...");
    test01();
    Console.printLine("test02...");
    test02();
    0

def test01(): Unit & Impure = 
    let ans = I18n/Locale.getIsoCountryCodes();
    List.foreach(println, ans) 

def test02(): Unit & Impure = 
    let ans = I18n/Locale.getIsoCountryCodesWithIsoCountryCode(I18n/Locale/IsoCountryCode.Part1Alpha2);
    Set.foreach(println, ans) 


