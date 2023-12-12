# ABCD 5.1 basic MRI observations
# http://dx.doi.org/10.15154/z563‑zd24

# basic longitudinal participant information
#
#   add age in years, remove time points of no interest (for SAIQ, which is only on annual follow-ups)

library(tidyverse)
library(readr)
library(reshape2)
library(patchwork)

source('./setupPaths_5.1.R')

source(file.path(toolsDir, "cmig_tools_utils/r/makeDEAPdemos.R"))
source(file.path(toolsDir, "cmig_tools_utils/r/makeDesign.R"))
datapath <- file.path(releaseDir, "abcd-general/")
deapdemos <- makeDEAPdemos(datapath) #this has 48807 yearly timepoints
#== add age in years
deapdemos = add_column(deapdemos, ageYrs = deapdemos$interview_age / 12, .after="interview_age")

mriData <- deapdemos[,c('src_subject_id', 'eventname','interview_age','ageYrs','sex')]

tableName <- 'mri_y_smr_area_dsk'
mriVars <- 'smri_area_cdk_total'
tmp <- read_csv(file.path(releaseDir, 'imaging', paste0(tableName, '.csv')), guess_max = 25000)
tmp <- tmp[, c('src_subject_id', 'eventname', mriVars)]
mriData <- right_join(mriData, tmp) # NB filter on mri rows, since only at even years
mriData[[mriVars]] <- mriData[[mriVars]] / 1000 #scale

tableName <- 'mri_y_smr_thk_dsk'
mriVars <- 'smri_thick_cdk_mean'
tmp <- read_csv(file.path(releaseDir, 'imaging', paste0(tableName, '.csv')), guess_max = 25000)
tmp <- tmp[, c('src_subject_id', 'eventname', mriVars)]
mriData <- join(mriData, tmp)

tableName <- 'mri_y_qc_raw_smr_t1'
mriVars <- paste0('iqc_t1_', 1:3, '_qc_score')
tmp <- read_csv(file.path(releaseDir, 'imaging', paste0(tableName, '.csv')), guess_max = 25000)
tmp <- tmp[, c('src_subject_id', 'eventname', mriVars)]
mriData <- join(mriData, tmp)

tableName <- 'mri_y_qc_raw_smr_t2'
mriVars <- paste0('iqc_t2_', 1:3, '_qc_score')
tmp <- read_csv(file.path(releaseDir, 'imaging', paste0(tableName, '.csv')), guess_max = 25000)
tmp <- tmp[, c('src_subject_id', 'eventname', mriVars)]
mriData <- join(mriData, tmp)


# spaghetti plots
adata <- mriData
adata[adata$eventname=='baseline_year_1_arm_1','tpname'] = 'BL'
adata[adata$eventname=='1_year_follow_up_y_arm_1','tpname'] = 'Y1'
adata[adata$eventname=='2_year_follow_up_y_arm_1','tpname'] = 'Y2'
adata[adata$eventname=='3_year_follow_up_y_arm_1','tpname'] = 'Y3'
adata[adata$eventname=='4_year_follow_up_y_arm_1','tpname'] = 'Y4'

events = c('BL','Y1','Y2','Y3','Y4')

#function to create a spaghetti plot of change in variable y across timepoints
tpPlot <- function(x,y,doLog, title, ylim) {
  p <- adata %>%
    group_by(src_subject_id) %>%
    ggplot(aes(x=.data[[x]],y=.data[[y]],group=src_subject_id)) +
    geom_line(alpha=.1, lwd=0.5, color='blue') +
    #geom_point(aes(x=.data[[x]],y=.data[[y]]), size=0.1, alpha=0.02) +
    #stat_smooth(aes(group=1),method="loess") +
    labs(title=title, y=y, x=x) +
    theme(plot.title=element_text(face="bold", size=15)) +
    theme_classic() + ylim(ylim)
  #geom_jitter(width=0.2)

  if (doLog) p <- p + scale_y_log10(labels = function(x) format(x, drop0trailing=T, scientific = FALSE))

  p
}

x="ageYrs"
y='smri_area_cdk_total'
title=paste0("Total Cortical Area")
doLog = FALSE
ylim <- c(120,260)
tpPlot(x,y,doLog, title, ylim)

x="ageYrs"
y='smri_thick_cdk_mean'
title=paste0("Mean Cortical Thickness")
doLog = FALSE
ylim <- c(2.2,3.4)
tpPlot(x,y,doLog, title, ylim)
