# =========================================================================
# Init machine-dependent files and directories ABCD 5.1 -------------------
# =========================================================================

#main defines of interest:
# resultDir
# abcdsyncDir
# dataDir
# tstamp
# scriptDir


library(jsonlite)
cfg <- fromJSON('~/abcdConfig.json')

abcd_doi <- '10.15154/z563‑zd24'

# set up directories for different hosts
host = Sys.info()["nodename"]
if ( pmatch("amusing", host, nomatch=FALSE) ) {
  scriptDir <- file.path(dataDir,"..","code/R")
  abcdsyncDir <- "C:/Users/jiversen/Documents/Data/abcd-sync"
  dataDir <- "C:/Users/jiversen/Documents/Data"
  resultDirRoot = file.path(dataDir,"results")

} else if ( pmatch("taiko", host, nomatch=FALSE) ) {
  dataDir <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/Data"
  abcdsyncDir <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/Data/abcd-sync"
  #dataDir <- "/Volumes/taiko2/ABCD 3.0/"
  scriptDir <- file.path(dataDir,"..","code/R")
  resultDirRoot = file.path(dataDir,"results")

} else if ( pmatch("ip", host, nomatch=FALSE) ) {
  abcdsyncDir <-cfg$data$abcd_sync
  dataDir <- '/home/jiversen/Data'
  scriptDir <- "/home/jiversen/R/saiq"
  resultDirRoot <- file.path("/home/jiversen/Results/saiq")

} else if ( pmatch("helmholtz", host, nomatch=FALSE) | pmatch("proteus", host, nomatch=FALSE) ) { # NB * This is the only up to date section for 5.0 (Nov 23)
  abcdsyncDir <-cfg$data$abcd_sync
  dataDir <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/Data"
  releaseDir <- file.path(dataDir,"/ABCD_release_5.1/abcd-data-release-5.1/core")
  scriptDir <- file.path("/Users/jri/Documents/ Research/Projects/simphony/ABCD/Code/ABCD-saiq-music")
  toolsDir <- "/Users/jri/Documents/matlab/matlab/projects/simphony/ABCD/cmig-research-group/cmig_tools_internal"
  resultDirRoot = file.path(dataDir,"results")
}

#from July 15, 2020--still valid?
prsFile <- file.path(dataDir,"merged_prs_scores.tsv")
pcsFile <- file.path(dataDir,"plink2.eigenvec")
