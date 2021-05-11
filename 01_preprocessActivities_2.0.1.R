
# Step 1: preprocess from raw NDA*.Rds to activities data
# Preprocess ABCD Activities questionnaire from DEAP 2.0.1 Rds file (doi:10.15154/1504431).
#   Subset, omitting columns of no (present) interest
#   Simplify variable naming
#   Convert activity amounts to numeric quantities and create aggregate 'intensity' measures
#   Create aggregate 'all sports' and 'all arts' activities
#   New (6/2020): add  reading and music listening as activities
#
# Source: nda*.Rds
# Product: nda*_activities.Rds         #activity table
#          nda*_activities_long.Rds    #long form
#          nda*_activities_meta.RData  #some convenience variables: subjVars, activities, measurements, timestamp

#version 2 -- add additional phenotypes besides basic demographic set

rm(list=ls())

ts <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
scriptName <- "01_preprocessActivities_2.0.1.R"

# =========================================================================
# Settings ----------------------------------------------------------------
# =========================================================================

# I/O
host = Sys.info()["nodename"]
if ( pmatch("amusing", host, nomatch=FALSE) ) {
  dataDir <- "C:/Users/jiversen/Documents/RDS/"
} else if ( pmatch("taiko", host, nomatch=FALSE) ) {
  dataDir <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/Data/"
}
scriptDir <- file.path(dataDir,"..","code/R")


ndaFile <- file.path(dataDir,"ABCD_releases_2.0.1_Rds/nda2.0.1.Rds")
ndaFileBaselineOnly <- file.path(dataDir,"ABCD_releases_2.0.1_Rds/nda2.0.1_baseline.Rds")

prsFile <- file.path(dataDir,"merged_prs_scores.tsv")
pcsFile <- file.path(dataDir,"plink2.eigenvec")

resultDir = file.path(dataDir,"results",ts)
dir.create(resultDir,recursive=TRUE,mode="0775")
file.copy(file.path(scriptDir,scriptName), file.path(resultDir,scriptName)) #copy this script to output directory for replicability

outFile      <- file.path(resultDir, "nda2.0.1_activities.Rds")
outFileLong  <- file.path(resultDir, "nda2.0.1_activities_long.Rds")
outFileMeta  <- file.path(resultDir, "nda2.0.1_activities_meta.RData")
outFileMusic <- file.path(resultDir, "nda2.0.1_activities_music.csv")

# log all output when sourcing

rname <-tryCatch(sys.frame(1)$filename, error=function(cond) {return(NULL)} ) #only log when sourced
isSourced <- (!is.null(rname))
if ( isSourced ) {
  logFile = paste0(resultDir, "/log_", basename(rname),"_",ts,".txt")
  sink(file=logFile, split=TRUE)
  writeLines(sprintf("\n======================================\nRunning: %s\n\tOn: %s\n",rname, ts))
}

# Analysis parameters
onlyBaseline = TRUE   # restrict to baseline timepoint
doImputeDK = FALSE

writeLines(sprintf("\nParameters:\n\tonlyBaseline=%s\n\tdoImputeDK=%s\n\n",onlyBaseline,doImputeDK))

# Per-subject descriptive/demographic variables and covariates
subjVars <- c("src_subject_id","eventname","abcd_site","rel_family_id","rel_group_id","age", "sex","household.income","high.educ","anthro_bmi_calc","race_ethnicity",'EUR', 'AFR', 'EAS', 'AMR',"accult_phenx_q1_p","accult_phenx_q1")

rsVars <- c("rsfmri_cor_network.gordon_auditory_network.gordon_auditory","rsfmri_cor_network.gordon_auditory_network.gordon_cingulooperc","rsfmri_cor_network.gordon_auditory_network.gordon_cinguloparietal","rsfmri_cor_network.gordon_auditory_network.gordon_dorsalattn","rsfmri_cor_network.gordon_auditory_network.gordon_frontoparietal", "rsfmri_cor_network.gordon_auditory_network.gordon_none","rsfmri_cor_network.gordon_auditory_network.gordon_retrosplenialtemporal" ,"rsfmri_cor_network.gordon_auditory_network.gordon_smhand","rsfmri_cor_network.gordon_auditory_network.gordon_smmouth","rsfmri_cor_network.gordon_auditory_network.gordon_salience","rsfmri_cor_network.gordon_auditory_network.gordon_ventralattn","rsfmri_cor_network.gordon_auditory_network.gordon_visual")

phenoVarPat <- "nihtbx_.+_uncorrected"

neurocogVarPc <- c("neurocog_pc1.bl", "neurocog_pc2.bl", "neurocog_pc3.bl" )

# music leisure listening and reading
musicListenVars <- c("sports_activity_lmusic_p","sports_activity_lmusic_years_p","sports_activity_lmusic_dk_p",
                    "sports_activity_lmusic_hours_p","sports_activity_lmusic_hours_dk_p","scrn_hr_music")

readVars <- c("sports_activity_read_p","sports_activity_read_years_p","sports_activity_read_years_dk_p",
              "sports_activity_read_hours_p","sports_activity_read_hours_dk_p")

# SAIQ Activity variables (present for each activity)
measurements <- c("school",	"outside",	"private",	"self",	"nyr",	"nmonth",	"perwk",	"tspent",	"p12")

# =========================================================================
# Preamble ----------------------------------------------------------------
# =========================================================================

library(dplyr)
library(plyr)
library(tidyr)
library(data.table)
library(tibble)
library(gsubfn)

#library(dplyr)
#library(rlang)
#library(corrplot)


# =========================================================================
# Load and subset ---------------------------------------------------------
# =========================================================================

# load NDS
if ( onlyBaseline & file.exists(ndaFileBaselineOnly)) {
  print(sprintf("Loading Rds: %s", ndaFileBaselineOnly))
  D <- readRDS(ndaFileBaselineOnly)
  names(D)[names(D)=="subjectid"] = "src_subject_id"
  gc()
} else {
  print(sprintf("Loading Rds: %s", ndaFile))
  D <- readRDS(ndaFile)

  #select baseline timepoint
  if (onlyBaseline) {
    #isBase = grepl("^baseline_year_1_arm_1$",D$eventname)
    #D = D[isBase,]
    D = D[D$eventname == "baseline_year_1_arm_1", ]
  }
  names(D)[names(D)=="subjectid"] = "src_subject_id"
  gc()
  saveRDS(D,ndaFileBaselineOnly)
  gc()
}

# add some additional information
if (length(prsFile) > 0) {

  # Helper function to reformat ids from PRS file (from univariate_models.r)
  reformat_id <- function(id){
    ans = unlist(strsplit(id, 'NDAR_'))[2]
    ans = paste0('NDAR_', unlist(strsplit(ans, '_'))[1])
    return(ans)
  }

  prs = read.table(prsFile, header=TRUE)
  colnames(prs)[1] = 'src_subject_id'
  reformated_rows = lapply(as.character(prs$src_subject_id), reformat_id)
  prs = prs[!duplicated(reformated_rows),]
  prs$src_subject_id = reformated_rows[!duplicated(reformated_rows)]
  # Concatonate _PRS to cols
  # colnames(prs)[2:dim(prs)[2]] = paste0(colnames(prs)[2:dim(prs)[2]], '_PRS')
  # Join with data frame
  D = join(D, prs, by='src_subject_id')
  subjVars <- c(subjVars, names(prs)[-1])
}

# Read in Genetic PCs if they are given
if (length(pcsFile) > 0){
  # Read PCs
  pcs = read.table(pcsFile, header=TRUE)
  colnames(pcs)[colnames(pcs)=='IID'] = 'src_subject_id'
  D = join(D, pcs, by='src_subject_id')
  subjVars <- c(subjVars, names(pcs)[-1:-2])
}

#match variable patterns
phenoVars <- names(D)[grep(phenoVarPat,names(D))]

subjVars <- c(subjVars, phenoVars, neurocogVarPc, rsVars) #hail mary
print("Subject Vars: ")
print(subjVars)

#library(data.table)
#fwrite(D, file=paste0(dataDir,"/results/nda2.0.1_baseline.csv")) #takes ~2 minutes, 7+GB

#Make a few sub-tables

#= demographic
Dsubj = D[,subjVars]
#== add age in years
Dsubj = add_column(Dsubj, ageYrs = Dsubj$age / 12, .after="age")
#== rename language vars
names(D)[names(D)=="accult_phenx_q1"] = "english_skill_self"
names(D)[names(D)=="accult_phenx_q1_p"] = "english_skill_parent"

#= lmusic and read for later
Dmr = D[, c(musicListenVars, readVars)]

#= SAIQ
saiqCols <- grepl("^sports_activity_",names(D)) & !grepl("_ss_",names(D)) & !grepl("_l$",names(D)) & !grepl("_activities_p_",names(D)) & !grepl("_select_language_",names(D)) & !names(D) %in% c(musicListenVars,readVars)
D = D[,saiqCols]

#== clean up saiq field names
names(D) = sub("sports_activity_","",names(D)) #remove the prefix from SAIQ
names(D) = sub("_p$","",names(D)) #remove any _p suffix *NB--be sure this does not create clash with youth version (if there will be one in future)--it would for accult_phenx, for ex.

names(Dmr) = sub("sports_activity_","",names(Dmr)) #remove the prefix from SAIQ
names(Dmr) = sub("_p$","",names(Dmr))
names(Dmr)[names(Dmr)=="lmusic_dk"] = "lmusic_years_dk" #fix a mis-named variable
#we'll further process these below...

#== fix some oddities in SAIQ -- doubled up fields for skate and sboard - p___0 is all the No's and p___1 all the Yes's with 'not endorsed' in the rest. Is there a difference between 'no' and 'not endorsed'? I think not, but preserve as NA
skate_p12 <-  rep(NA,nrow(D))
skate_p12[D$skate_p12_p___0=="No"] = "No"
skate_p12[D$skate_p12_p___1=="Yes"] = "Yes"
D = add_column(D, skate_p12 = as.factor(skate_p12), .before="skate_p12_p___1")

sboard_p12 <-  rep(NA,nrow(D))
sboard_p12[D$sboard_p12_p___0=="No"] = "No"
sboard_p12[D$sboard_p12_p___1=="Yes"] = "Yes"
D = add_column(D, sboard_p12 = as.factor(sboard_p12), .before="sboard_p12_p___1")

D <- D[,!names(D) %in% c("skate_p12_p___0",  "skate_p12_p___1", "sboard_p12_p___0", "sboard_p12_p___1")]

#== rename m_arts without underscore
D <- setnames(D,old=paste0('m_arts_', measurements), new=paste0('marts_',measurements))

# #select baseline timepoint
# if (onlyBaseline) {
#   isBase = grepl("^baseline_year_1_arm_1$",Dsubj$eventname)
#   Dsubj = Dsubj[isBase,]
#   Dmr = Dmr[isBase,]
#   D = D[isBase,]
# }
# gc()

#=========================================================================
# Expand activities characterization -------------------------------------
#=========================================================================

# handle "Don't know" answers in one of two ways: 1) set to NA, 2) replace with mean of valid answers
#  the logic here is DK is perhaps different than 'no answer', because we know they do an activity. The # of DKs is usually _very_ small
# NOTE: function assumes all 'numeric' factors except for "Don't know", so convert any other strings factors to numbers before calling (e.g. "60 (1 hr)" -> "60")
handleDK <- function(data, varname, doImpute) {
  DK = c("Don't know") #add multiple values if needed (hopefully not)
  tmp = data[,varname]
  iDK = which(tmp %in% DK)
  tmp[iDK] <- NA
  print(sprintf("DK:  %d in %s",length(iDK), varname))
  tmp = as.numeric(as.character(tmp)) #correct way to convert numeric factors to numeric. NOTE: a NA coersion warning is sign that assumption is violated
  if (doImpute) {
    mm = mean(tmp[-iDK], na.rm=TRUE)
    tmp[iDK] <- mm
    print(sprintf("   Imputing DK to mean (%.2f)", mm))
  }
  return(tmp)
}

#convert "Yes"/"No" factor to TRUE/FALSE
yn2tf <- function(data) {
  data <- as.factor(data)
  levels(data)[levels(data)=="Yes"] <- TRUE
  levels(data)[levels(data)=="No"] <- FALSE
  return( as.logical(data) )
}

#== Loop over the activities, calculating additional useful variables per-activity, and building an actData table

#== list of all activities
activities <- unlist(strapplyc(names(D),"(.*)_school$"))

actData <- Dsubj #start with subject-level descriptors

for (act in activities) {

  print(sprintf("== %s ==", act))
  actVars <- paste0(act,"_",measurements)
  adata <- D[,actVars]

  #activity contexts
  #convert Yes/No to 1/0
  ynVars <- c("school","outside","private","self","p12")
  adata[,paste0(act,"_",ynVars)] <- apply(adata[,paste0(act,"_",ynVars)], 2, yn2tf)

  # helper variable that is true if activity is endorsed in _any_ context
  #adata[paste0(act,"_any")] <- apply(adata[,paste0(act,"_",c("school","outside","private","self"))], 1, function (x) as.factor(ifelse(any(x=="Yes", na.rm=TRUE),"Yes",ifelse(any(x=="No", na.rm=TRUE),"No",NA))))
  # without usual na.rm, preserves FALSE meaning they endorsed it, but  all contexts are "No", which is bogus, vs did not endorse
  adata[paste0(act,"_any")] <- apply(adata[,paste0(act,"_",c("school","outside","private","self"))], 1, any)

  #fix up codes and convert to numeric.
  # === nyr ===
  var = paste0(act,"_nyr")
  adata[,var] = handleDK(adata, var, doImputeDK)

  # add an approximate 'age started' variable
  # NB for dance, e.g., some 10 gave nyrs response suggesting they began < 1 year old, and 3 that they began before birth.
  # Question: are these completely bogus, or can we assume these are kids who have been doing it for a long time and the parents overshot a little?

  as <-  actData$ageYrs - adata[,var]
  asVar <- paste0(act,'_ageStarted')
  if (sum(as<1,na.rm=TRUE))
    print(sprintf("ANOMALOUS: %s < {1, 0} years in {%d, %d} respondents",asVar, sum(as<1,na.rm=TRUE), sum(as<0,na.rm=TRUE)))
  #as[as<0] <- NA # Leave data as is, but report and remember to filter during analysis
  adata[,asVar] <- as

  # === nmonth ===
  var = paste0(act,"_nmonth")
  adata[,var] = handleDK(adata, var, doImputeDK)

  #Catch any bogus values (should be impossible)
  if (sum(adata[,var] > 12, na.rm=TRUE))
    print(sprintf("ANOMALOUS %s: %d nmonth > 12.",var, sum(adata[,var] > 12, na.rm=TRUE)))

  #convert perwk and tspent to continuous quantities
  # === perwk: days per week ===
  var <- paste0(act,"_","perwk")
  tmp <- adata[,var]
  levels(tmp)[levels(tmp)=="Once every 2 weeks"] <- 0.5 #Once every 2 weeks
  levels(tmp)[levels(tmp)=="One day every month"] <- 0.25 #One day a month (once every 4 weeks)
  levels(tmp)[levels(tmp)=="Less than one day per month"] <- 0.1 #less than one day a month (arbitrary, small number akin to once every 2.5 months)
  adata[,var] <- tmp
  adata[,var] = handleDK(adata, var, doImputeDK)

  #Catch bogus values
  if (sum(adata[,var] > 7, na.rm=TRUE))
    print(sprintf("%s: %d perwk > 7",var, sum(adata[,var] > 7, na.rm=TRUE)))

  # === tspent: minutes per session - convert from code to actual minutes ===
  # some assumptions here at extremes; <30 minutes --> 15; >3 hours --> 240 (4 hours)
  var <- paste0(act,"_","tspent")
  tmp <- adata[,var]
  levels(tmp)[levels(tmp)=="less than 30 minutes"] <- 15 #NB: assumption
  levels(tmp)[levels(tmp)=="60 (1 hr)"] <- 60
  levels(tmp)[levels(tmp)=="90 (1.5 hrs)"] <- 90
  levels(tmp)[levels(tmp)=="120 (2 hrs)"] <- 120
  levels(tmp)[levels(tmp)=="150 (2.5 hrs)"] <- 150
  levels(tmp)[levels(tmp)=="180 (3 hrs)"] <- 180
  levels(tmp)[levels(tmp)=="greater than 3 hours"] <- 240 #NB: assumption
  adata[,var] <- tmp
  adata[,var] = handleDK(adata, var, doImputeDK)


  # create  aggregate intensity measures: mean hours per week (across the year), lifetime total hours (assumes constant hours/week)
  # tspent*perwk*(nmonth/12) / 60
  adata[,paste0(act,"_hrperwk")] <- adata[paste0(act,"_tspent")] * adata[paste0(act,"_perwk")] * (adata[paste0(act,"_nmonth")]/12) / 60
  # tspent*perwk*4*nmonth*nyr / 60
  adata[,paste0(act,"_hrlifetime")] <- adata[paste0(act,"_tspent")] * adata[paste0(act,"_perwk")] * 4 * adata[paste0(act,"_nmonth")] * adata[paste0(act,"_nyr")] / 60

  # concatenate this expanded activity data onto the tabler
  actData <- cbind(actData, adata)

} # loop over activities


#=========================================================================
# Create lmusic and read 'pseudo' activities -----------------------------
#=========================================================================

# music leisure listening and reading have a reduced set of measures, but we'll convert into full activities by assuming they are done in context 'self', 12 months/year, 'hours' is tspent*perwk,
# with no way to know how it's divided up, so just use the hrperwk value.
# TODO

# =========================================================================
# Add aggregate participation 'activities': all Sports, all Arts ----------
# =========================================================================

print("== Adding aggregate sports & arts ==")

sports <- c("base","basket","climb","fhock","fball","gym","ihock","polo","iskate","marts","lax","rugby","skate","sboard","soc","surf","wpolo","tennis","run","mma","vball","yoga")
actData$sports_any <-  apply(actData[,paste0(sports,"_any")], 1, any,na.rm=TRUE) #Note: apply is necessary here to force row-wise calculations
actData$sports_p12 <- apply(actData[,paste0(sports,"_p12")], 1, any,na.rm=TRUE)
actData$sports_school <- apply(actData[,paste0(sports,"_school")], 1, sum,na.rm=TRUE)
actData$sports_outside <- apply(actData[,paste0(sports,"_outside")], 1, sum,na.rm=TRUE)
actData$sports_private <- apply(actData[,paste0(sports,"_private")], 1, sum,na.rm=TRUE)
actData$sports_self <- apply(actData[,paste0(sports,"_self")], 1, sum,na.rm=TRUE)
actData$sports_nyr <- apply(actData[,paste0(sports,"_nyr")], 1, mean,na.rm=TRUE)
actData$sports_nmonth <- apply(actData[,paste0(sports,"_nmonth")], 1, mean,na.rm=TRUE)
actData$sports_perwk <- apply(actData[,paste0(sports,"_perwk")], 1, mean,na.rm=TRUE)
actData$sports_tspent <- apply(actData[,paste0(sports,"_tspent")], 1, mean,na.rm=TRUE)
actData$sports_hrperwk <- apply(actData[,paste0(sports,"_hrperwk")], 1, sum,na.rm=TRUE)
actData$sports_hrlifetime <- apply(actData[,paste0(sports,"_hrlifetime")], 1, sum,na.rm=TRUE)

arts <- c("dance","music", "art", "drama", "crafts")
actData$arts_any <-  apply(actData[,paste0(arts,"_any")], 1, any,na.rm=TRUE)
actData$arts_p12 <- apply(actData[,paste0(arts,"_p12")], 1, any,na.rm=TRUE)
actData$arts_school <- apply(actData[,paste0(arts,"_school")], 1, sum,na.rm=TRUE)
actData$arts_outside <- apply(actData[,paste0(arts,"_outside")], 1, sum,na.rm=TRUE)
actData$arts_private <- apply(actData[,paste0(arts,"_private")], 1, sum,na.rm=TRUE)
actData$arts_self <- apply(actData[,paste0(arts,"_self")], 1, sum,na.rm=TRUE)
actData$arts_nyr <- apply(actData[,paste0(arts,"_nyr")], 1, mean,na.rm=TRUE)
actData$arts_nmonth <- apply(actData[,paste0(arts,"_nmonth")], 1, mean,na.rm=TRUE)
actData$arts_perwk <- apply(actData[,paste0(arts,"_perwk")], 1, mean,na.rm=TRUE)
actData$arts_tspent <- apply(actData[,paste0(arts,"_tspent")], 1, mean,na.rm=TRUE)
actData$arts_hrperwk <- apply(actData[,paste0(arts,"_hrperwk")], 1, sum,na.rm=TRUE)
actData$arts_hrlifetime <- apply(actData[,paste0(arts,"_hrlifetime")], 1, sum,na.rm=TRUE)

actData$musicNoSport_any <- actData$music_any>0 & !actData$sports_any>0
actData$musicNoSport_p12 <- actData$music_p12 & !actData$sports_p12

#========================================================================= ==  =
# save -------------------------------------------------------------------------\/\/\/\/\
#========================================================================= ==  =

print(sprintf("== Saving: %s", outFile))
timestamp = Sys.time()
saveRDS(actData, file=outFile)

#save some convenience variables
print(sprintf("== Saving: %s", outFileMeta))
save(subjVars,activities,measurements,timestamp,file=outFileMeta)

#save long-form (in activities) table, with a column for each measure
# Note, need to be careful to update the column exclusion if adding more non-activity vars
actDataL <- actData %>%
  gather("activity_measure", "Value", -src_subject_id:-rsfmri_cor_network.gordon_auditory_network.gordon_visual ) %>%
  separate(activity_measure, c("activity", "measure"), sep='_', remove=TRUE) %>%
  spread(measure, Value)
actDataL <- select(actDataL, match(c(subjVars,"activity","any",measurements,"hrperwk","hrlifetime"),names(actDataL)))

print(sprintf("== Saving: %s", outFileLong))
saveRDS(actDataL, file=outFileLong)

print(sprintf("== Saving: %s", outFileMusic))
act = "music"
actVars <- paste0(act,"_",measurements)
adata <- actData[,actVars]
musicData <- cbind(Dsubj, adata)
fwrite(musicData,file = outFileMusic)

print("Done.")

#close the log
if (isSourced)
  sink()
