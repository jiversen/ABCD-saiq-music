#library loader preamble--make sure all usual libs are installed
#  first implementation is a vectorized version from DEAP;
#  second is based on require returning false if a library isn't available; could have done anonymous function, but this is clearer

mypack <- c("dplyr", "dtplyr", "tidyr", "data.table", "gsubfn", "rlang",
            "corrplot", "rjson", "knitr", "R.matlab", "gamm4","devtools",
            "plyr","stringr","MuMIn","pracma","psych","forecast","car",
            "tibble")

#3.6.3 psych needs older install.packages("~/Downloads/mnormt_1.5-7.tgz", repos = NULL, type = .Platform$pkgType)
# devtools::install_github("tylermorganwall/rayshader")
# needs "magick", "sf", "ambient"

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
