# ABCD 5.1 nc_y_nihtb table
# http://dx.doi.org/10.15154/z563‑zd24

# basic longitudinal participant information
#
#   add age in years, remove time points of no interest (for SAIQ, which is only on annual follow-ups)

library(tidyverse)
library(readr)
library(reshape2)
library(patchwork)

source('./setupPaths_5.1.R')

nc_y_nihtb <- read_csv(file.path(releaseDir, "neurocognition/nc_y_nihtb.csv"),guess_max = 25000)


