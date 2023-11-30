# ABCD 5.0 mc_y_nihtb table
# basic longitudinal participant information
#
#   add age in years, remove time points of no interest (for SAIQ, which is only on annual follow-ups)

library(tidyverse)
library(readr)
library(reshape2)
library(patchwork)

source('./setupPaths_5.0.R')

nc_y_nihtb <- read_csv(file.path(dataDir, "neurocognition/nc_y_nihtb.csv"),guess_max = 25000)


