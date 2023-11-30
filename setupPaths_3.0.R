# =========================================================================
# Init machine-dependent files and directories ABCD 3.0 -------------------
# =========================================================================

#main defines of interest:
# resultDir
# ndaFile
# dataDir
# tstamp

tstamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

library(jsonlite)
cfg <- fromJSON('~/abcdConfig.json')

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
}

ndaFile <- file.path(abcdsyncDir,cfg$info$dataRelease,'RDS','nda3.0.Rds')
ndaFileBaselineOnly <- file.path(dataDir,"nda3.0_baseline.Rds")

prsFile <- file.path(dataDir,"merged_prs_scores.tsv")
pcsFile <- file.path(dataDir,"plink2.eigenvec")

dir.create(resultDir,recursive=TRUE,mode="0775")

outFileBase  <- file.path(resultDir, "nda3.0_activities")
