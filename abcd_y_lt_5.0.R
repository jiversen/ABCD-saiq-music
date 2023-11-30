# ABCD 5.0 abcd_y_lt table
# http://dx.doi.org/10.15154/8873-zj65
#
# basic longitudinal participant information
#
#   add age in years, remove time points of no interest (for SAIQ, which is only on annual follow-ups)

library(tidyverse)
library(readr)
library(reshape2)
library(patchwork)

source('./setupPaths_5.0.R')

# skip school and birth IDs (for now)

abcd_y_lt <- read_csv(file.path(dataDir, "abcd-general/abcd_y_lt.csv"),
                      #col_types = 'fff----?nf')
                      col_types = list(
                        src_subject_id = col_factor(),
                        eventname = col_factor(),
                        site_id_l = col_factor(),
                        rel_family_id = col_factor(),
                        rel_birth_id = col_skip(),
                        school_id = col_skip(),
                        district_id = col_skip(),
                        interview_date = col_date(format='%m/%d/%Y'),
                        interview_age = col_double(),
                        visit_type = col_factor()
                      )
)

#== add age in years
abcd_y_lt = add_column(abcd_y_lt, ageYrs = abcd_y_lt$interview_age / 12, .after="interview_age")
