#library loader preamble
#  first implementation is a vectorized version from DEAP; 
#  second is based on require returning false if a library isn't available; could have done anonymous function, but this is clearer

mypack <- c("dplyr", "dtplyr", "tidyr", "data.table", "gsubfn", "rlang", 
            "corrplot", "rjson", "knitr", "R.matlab", "gamm4")

#junk <- lapply(mypack, function(x) if (!(x %in% installed.packages()[,"Package"])) install.packages(x) )
#junk <- lapply(mypack, function(x) if (!(x %in% .packages())) library(x, character.only = TRUE) )

library.install <- function(l) {
  doit <- function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x)
      library(x, character.only = TRUE)
    }
  }
  invisible(lapply(l, doit))
}

library.install(mypack)
