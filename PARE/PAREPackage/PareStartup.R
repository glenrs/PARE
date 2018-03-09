library("devtools")
library(roxygen2)

setwd("./PARE")
document()

setwd("..")
install("PARE")

build("PARE")


