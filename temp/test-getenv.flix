
def main(_args: Array[String]): Int32 & Impure = 
    println("Running ${Time/LocalTime.now()}...");
    println("test01...");
    test01();
    0

def test01(): Unit & Impure = 
    match System/Environment.getEnvVars() {
        case Some(x1) => Map.foreach((k, v) -> println("${k}: ${v}"), x1)
        case None => println("Error - no envvars")
    }
