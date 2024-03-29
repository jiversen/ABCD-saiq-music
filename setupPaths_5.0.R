# =========================================================================
# Init machine-dependent files and directories ABCD 5.0 -------------------
# =========================================================================

#main defines of interest:
# resultDir
# abcdsyncDir
# dataDir
# tstamp
# scriptDir

tstamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

library(jsonlite)
cfg <- fromJSON('~/abcdConfig.json')

abcd_doi <- '10.15154/8873‑zj65'

# set up directories for different hosts
host = Sys.info()["nodename"]
if ( pmatch("amusing", host, nomatch=FALSE) ) {
  scriptDir <- file.path(dataDir,"..","code/R")
  abcdsyncDir <- "C:/Users/jiversen/Documents/Data/abcd-sync"
  dataDir <- "C:/Users/jiversen/Documents/Data"
  resultDir = file.path(dataDir,"results",tstamp)

} else if ( pmatch("taiko", host, nomatch=FALSE) ) {
  dataDir <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/Data"
  abcdsyncDir <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/Data/abcd-sync"
  #dataDir <- "/Volumes/taiko2/ABCD 3.0/"
  scriptDir <- file.path(dataDir,"..","code/R")
  resultDir = file.path(dataDir,"results",tstamp)

} else if ( pmatch("ip", host, nomatch=FALSE) ) {
  abcdsyncDir <-cfg$data$abcd_sync
  dataDir <- '/home/jiversen/Data'
  scriptDir <- "/home/jiversen/R/saiq"
  resultDir <- file.path("/home/jiversen/Results/saiq",tstamp)

} else if ( pmatch("helmholtz", host, nomatch=FALSE) | pmatch("proteus", host, nomatch=FALSE) ) { # NB * This is the only up to date section for 5.0 (Nov 23)
  abcdsyncDir <-cfg$data$abcd_sync
  dataDir <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/Data/ABCD_release_5.0/abcd-data-release-5.0/core/"
  scriptDir <- file.path("/Users/jri/Documents/ Research/Projects/simphony/ABCD/Code/ABCD-saiq-music/")
  resultDir = file.path("/Users/jri/Documents/ Research/Projects/simphony/ABCD/Data/results",tstamp)
}

#from July 15, 2020--still valid?
prsFile <- file.path(dataDir,"merged_prs_scores.tsv")
pcsFile <- file.path(dataDir,"plink2.eigenvec")
