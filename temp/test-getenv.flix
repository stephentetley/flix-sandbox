
def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running ${Time/LocalTime.now()}...");
    Console.printLine("test01...");
    test01();
    0

def test01(): Unit & Impure = 
    match System/Environment.getEnvVars() {
        case Some(x1) => Map.foreach((k, v) -> Console.printLine("${k}: ${v}"), x1)
        case None => Console.printLine("Error - no envvars")
    }
