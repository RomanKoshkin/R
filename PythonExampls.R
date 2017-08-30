library(rPython)
a <- 1:4
python.assign( "a", a )
python.exec("
a = a * 2
")

result = python.get("a")
python.exec( "import math" )
