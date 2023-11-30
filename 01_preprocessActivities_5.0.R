# OBSOLETE--moved to ph_p_saiq.R and others
# Step 1: preprocess from raw table download to activities data and associated demographic variables of interest
# http://dx.doi.org/10.15154/8873-zj65
#
#  a) download data from NDA ABCD 5.0 -> /Users/jri/Documents/ Research/Projects/ABCD/Data Downloads/ABCD 5.0/abcd-data-release-5.0/core
# Preprocess ABCD Activities questionnaire from DEAP 3.0 Rds file (doi:10.15154/1520591).
#   Subset, omitting columns of no (present) interest
#   Simplify variable naming
#   Convert activity amounts to numeric quantities and create aggregate 'intensity' measures
#   Create aggregate 'all sports' and 'all arts' activities
#   New (6/2020): add  reading and music listening as activities
#   New (2/2021): add second year
#
#     first year is in sports_activity_music_*_p from abcd_sqiq02 [**   used to be sai_p_*   **]
#       _nyr, _nmonth, _perwk, _tspent, p12, _self, _private, _outside, _school (all with _p suffix)
#
#     later years is in sports_acivity_music_*_p_l variables from abcd_lpsaiq01
#       _nmonth, _perwk, _tspent, _self, _private, _outside, _school, _self_pract (all with _p_l suffix)
#
#       sports_activity_music_instr_p___0: guitar, bass guitar, ukelele  #*** 1yr and 2yr (despite no _l in name)
#         1: flute, piccolo, recorder
#         2: clarinet, saxophone, oboe, bassoon
#         3: violin, viola
#         4: cello, bass
#         5: piano
#         6: harp
#         7: drums, percussion
#         8: trumpet, trombone, horns
#         9: vocal, singing
#        10: electric keyboard or organ
#        11: dj, electronic dance music edm
#        12: other
#       801: n/a
#
#   Note: endorsement variable is sports_activity_activities_p___23 and sports_activity_activities_p_l___23
#
#       summary(D[, 'sports_activity_activities_p___23'])
#           4752 endorsed, 7126 not endorsed, 42716 NA
#       summary(D[!is.na(D[, 'sports_activity_activities_p___23']),'eventname'])
#           11878 baseline_year_1__arm_1,    4752 / 11878 = 40%
#
#       summary(D[, 'sports_activity_activities_p_l___23'])
#           7144 endorsed, 10662 not endorsed, 36788 NA
#       summary(D[!is.na(D[, 'sports_activity_activities_p_l___23']),'eventname'])
#           11235 1_year_follow_up_y_arm_1,  4469 / 11235 = 39.8%  # summary(D[(D[,'eventname']=='1_year_follow_up_y_arm_1'), 'sports_activity_activities_p_l___23'])
#            6571 2_year_follow_up_y_arm_1,  2675 / 6571  = 40.7%  # summary(D[(D[,'eventname']=='2_year_follow_up_y_arm_1'), 'sports_activity_activities_p_l___23'])
#
# CHANGES from baseline to later years (longitudinal)
#   _nyr & _p12 only in baseline. p12 is of course implicit in the yearly follow-ups
#
#  NEW: sports_activity_music_self_pract_p_l [hours perwk]
#   "About how many hours per week does your child usually practice on their own?"
#     1 = Less than 1 hour ; 2 = 1 hour or more,but less than 2 hours ; 3 = 2 hours or more,but less than 3 hours ;
#     4 = 3 hours or more,but less than 4 hours ; 5 = 4 hours or more,but less than 5 hours ;
#     6 = 5 hours or more,but less than 6 hours ; 7 = 6 hours or more,but less than 8 hours ;
#     8 = 8 hours or more,but less than 12 hours ; 9 = 12 hours or more,but less than 18 hours ;
#     10 = 18 hours or more,but less than 24 hours; 11 = 24 hours or more,but less than 36 hours ;
#     12 = 36 hours or more,but less than 72 hours ; 13 = More than 72 hours ; 999 = Don't know
#
#   as of 3.0, this is not NA in 730 for 1_year_follow_up_y_arm_1, and 1635 for 2_year_follow_up_y_arm_1
#       why is it different? How many people were asked?
#     sp <- D[, grep('self_pract',names(D))]
#     summary(D[!is.na(sp),'eventname'])
#
# Listening vars ('listening to music for pleasure') sports_activiy_*
#   first year
#     lmusic_hours_p, lmusic_years_p, lmusic_p,
#
#   second year
#     lmusic_hours_p_l, lmusic_p_l
#
# OTHER
#   Vancouver Index of Acculturation abcd_via01
#     via_accult_q8_p 'I enjoy entertainment (e.g. movies, music) from my heritage culture'
#     via_accult_q9_p 'I enjoy typical America entertainment (e.g. movies, music)'
#
#   ABCD Screener abcd_screen01 ?? general behaviors??
#     scrn_hr_music 'plays a musical instrument now or within past 6 months': 2=very true/often, 1=sometimes, 0 not true

#
# Source: nda*.Rds
# Product: nda*_activities.Rds         #activity table
#          nda*_activities_long.Rds    #long form
#          nda*_activities_meta.RData  #some convenience variables: subjVars, activities, measurements, timestamp

#version 2 -- add additional phenotypes besides basic demographic set

#rm(list=ls())

# =========================================================================
# Settings ----------------------------------------------------------------
# =========================================================================

scriptName <- "01_preprocessActivities_5.0.R"

# Analysis parameters
onlyBaseline = FALSE # restrict to baseline timepoint
doImputeDK = FALSE

# =========================================================================
# Libraries ---------------------------------------------------------------
# =========================================================================

library(dplyr)
library(tidyr)
library(data.table)
library(tibble)
library(gsubfn)

# =========================================================================
# Variables of interest ---------------------------------------------------
# =========================================================================

# Per-subject descriptive/demographic variables and covariates
subjVars <- c("src_subject_id","eventname","interview_datetime","abcd_site","rel_family_id","rel_group_id","age", "sex","household.income","high.educ","anthro_bmi_calc","race_ethnicity","accult_phenx_q1_p","accult_phenx_q1")

rsVars <- c("rsfmri_cor_network.gordon_auditory_network.gordon_auditory","rsfmri_cor_network.gordon_auditory_network.gordon_cingulooperc","rsfmri_cor_network.gordon_auditory_network.gordon_cinguloparietal","rsfmri_cor_network.gordon_auditory_network.gordon_dorsalattn","rsfmri_cor_network.gordon_auditory_network.gordon_frontoparietal", "rsfmri_cor_network.gordon_auditory_network.gordon_none","rsfmri_cor_network.gordon_auditory_network.gordon_retrosplenialtemporal" ,"rsfmri_cor_network.gordon_auditory_network.gordon_smhand","rsfmri_cor_network.gordon_auditory_network.gordon_smmouth","rsfmri_cor_network.gordon_auditory_network.gordon_salience","rsfmri_cor_network.gordon_auditory_network.gordon_ventralattn","rsfmri_cor_network.gordon_auditory_network.gordon_visual")

phenoVarPat <- "nihtbx_.+_uncorrected"

neurocogVarPc <- c("neurocog_pc1.bl", "neurocog_pc2.bl", "neurocog_pc3.bl" )


# =========================================================================
# Helper Functions --------------------------------------------------------
# =========================================================================

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


# =========================================================================
# Init files and directories ----------------------------------------------
# =========================================================================
source('./setupPaths_5.0.R')
#defines resultDir, dataDir, tstamp
dir.create(resultDir,recursive=TRUE,mode="0775")
outFileBase  <- file.path(resultDir, "abcd_5.0_activities")
outFileAct   <- paste0(outFileBase, '.Rds')
outFileLong  <- paste0(outFileBase, '_long.Rds')
outFileMeta  <- paste0(outFileBase, '_meta.RData')
outFileMusic <- paste0(outFileBase, '_music.csv')
outFileSubj  <- paste0(outFileBase, '_subj.Rds')

# =========================================================================
# Start Logging -----------------------------------------------------------
# =========================================================================

#copy this exact script to output directory for replicability
file.copy(file.path(scriptDir,scriptName), file.path(resultDir,scriptName))

# log output when this code was sourced
rname <-tryCatch(sys.frame(1)$filename, error=function(cond) {return(NULL)} ) #only log when sourced
isSourced <- ( !is.null(rname) )
if ( isSourced ) {
  logFile = paste0(resultDir, "/log_", basename(rname),"_",tstamp,".txt")
  sink(file=logFile, split=TRUE)
  writeLines(sprintf("\n======================================\nRunning: %s\n\tOn: %s\n",rname, tstamp))
}

writeLines(sprintf("\nParameters:\n\tonlyBaseline=%s\n\tdoImputeDK=%s\n\n",onlyBaseline,doImputeDK))

# =========================================================================
# Load data from various tables and combine -------------------------------
# =========================================================================

# =========================================================================
# Table with standard per-subject vars ------------------------------------
# =========================================================================

Dsubj <- D[,subjVars]

#== add age in years
Dsubj = add_column(Dsubj, ageYrs = Dsubj$age / 12, .after="age")
#== rename language vars
names(Dsubj)[names(Dsubj)=="accult_phenx_q1"] = "english_skill_self"
names(Dsubj)[names(Dsubj)=="accult_phenx_q1_p"] = "english_skill_parent"

# =========================================================================
# Add Genetic PRS and PCs -------------------------------------------------
# =========================================================================

#outFileGen <- paste0(outFileBase, '_gen.Rds')

#Dgen <- D[,subjVars[1:2]] #start off with src_subject_id and event

# add some additional information if needed
if (0 & length(prsFile) > 0) {

  # Helper function to reformat ids from PRS file (from univariate_models.r)
  reformat_id <- function(id){
    ans = unlist(strsplit(id, 'NDAR_'))[2]
    ans = paste0('NDAR_', unlist(strsplit(ans, '_'))[1])
    return(ans)
  }

  prs = read.table(prsFile, header=TRUE)
  colnames(prs)[1] = 'src_subject_id'
  reformated_rows = lapply(as.character(prs$src_subject_id), reformat_id)
  prs$src_subject_id <- as.factor(reformated_rows)
  prs = prs[!duplicated(reformated_rows),]
  # Concatonate _PRS to cols
  # colnames(prs)[2:dim(prs)[2]] = paste0(colnames(prs)[2:dim(prs)[2]], '_PRS')
  # Join with data frame if needed
  if (! names(prs)[4] %in% names(D) ) {
    Dsubj = left_join(Dsubj, prs, by='src_subject_id')
    subjVars <- c(subjVars, names(prs)[-1])
  }
}

# Read in Genetic PCs if they are given
if (length(pcsFile) > 0){
  # Read PCs
  pcs = read.table(pcsFile, header=TRUE)
  colnames(pcs)[colnames(pcs)=='IID'] = 'src_subject_id'
  pcs[['FID']]<-NULL

  #join if needed
  if (! names(pcs)[4] %in% names(D) ) {
    Dsubj = left_join(Dsubj, pcs, by='src_subject_id')
    subjVars <- c(subjVars, names(pcs)[-1:-2])
  }
}

#saveRDS(Dgen, outFileGen)

# =========================================================================
# Add neurocog ------------------------------------------------------------
# =========================================================================

#match variable patterns
phenoVars <- names(D)[grep(phenoVarPat,names(D))]

Dsubj <- cbind(Dsubj,D[,c(phenoVars, neurocogVarPc)])
#subjVars <- c(subjVars, phenoVars, neurocogVarPc, rsVars) #hail mary
subjVars <- names(Dsubj)
print("Subject Vars: ")
print(subjVars)

#library(data.table)
#fwrite(D, file=paste0(dataDir,"/results/nda2.0.1_baseline.csv")) #takes ~2 minutes, 7+GB

saveRDS(Dsubj, outFileSubj)

# =========================================================================
# SAIQ  -------------------------------------------------------------------
# =========================================================================

#== fix some oddities in SAIQ -- doubled up fields for skate and sboard - p___0 is all the No's and p___1 all the Yes's with 'not endorsed' in the rest. Is there a difference between 'no' and 'not endorsed'? I think not, but preserve as NA
# =========================================================================
sports_activity_skate_p12_p <-  rep(NA,nrow(D))
sports_activity_skate_p12_p[D$sports_activity_skate_p12_p___0=="No"] = "No"
sports_activity_skate_p12_p[D$sports_activity_skate_p12_p___1=="Yes"] = "Yes"
D = add_column(D, sports_activity_skate_p12_p = as.factor(sports_activity_skate_p12_p), .before="sports_activity_skate_p12_p___1")

sports_activity_sboard_p12_p <-  rep(NA,nrow(D))
sports_activity_sboard_p12_p[D$sports_activity_sboard_p12_p___0=="No"] = "No"
sports_activity_sboard_p12_p[D$sports_activity_sboard_p12_p___1=="Yes"] = "Yes"
D = add_column(D, sports_activity_sboard_p12_p = as.factor(sports_activity_sboard_p12_p), .before="sports_activity_sboard_p12_p___1")

D <- D[,!names(D) %in% c("sports_activity_skate_p12_p___0",  "sports_activity_skate_p12_p___1", "sports_activity_sboard_p12_p___0", "sports_activity_sboard_p12_p___1")]


# == SAIQ constants
# =========================================================================

# SAIQ Activity variables (present for each activity)
# NB nyr, and p12 present only at baseline, we will synthesize in later years: nyr = nyr_priortimepoint+endorsed; p12 = endorsed
measurements <- c("school","outside","private","self","nyr","nmonth","perwk","tspent","p12")
baseOnlyIdx <- c(5,9)

# activities conversion table
activityName <- c('Dance','Baseball','Basketball','Climbing','Field Hockey','Football','Gymnastics','Ice Hockey','Horseback','Skating','Martial Arts','Lacrosse','Rugby','Skateboard',
                  'Snowsports','Soccer','Surfing','Swimming','Tennis','Running','Wrestling','Volleyball','Yoga','Music','Visual Art','Drama','Crafts','Games','Collecting','None' )
activityField <- c('dance','base','basket','climb','fhock','fball','gym','ihock','polo','iskate','m_arts','lax','rugby','skate','sboard','soc','surf','wpolo','tennis','run',
                   'mma','vball','yoga','music','art','drama','crafts','chess','collect','?None?')
activityCode <- as.character(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)) #seq(0,29), but explicit because verified in dictionary
activitiesTable <- data.frame(Activity = activityName, Field = activityField, stringsAsFactors=FALSE)
rownames(activitiesTable) <- activityCode
# helper to look up activity name from variable name
act.name <- function(field) {
  return ( activitiesTable$Activity[activitiesTable$Field == field] )
}

# merge baseline and longitudinal, convert various categoricals to numeric
# =========================================================================

#create selection vectors for timepoints with SAIQ surveys, use to look up 'previous' value for nyr
tpidx <- list()
saiqEventnames <- c('baseline_year_1_arm_1','1_year_follow_up_y_arm_1','2_year_follow_up_y_arm_1')
en <- D[,'eventname']
for (i in 1:length(saiqEventnames)) {
  tpidx[i] <- list(grepl(saiqEventnames[i], en))
}
names(tpidx) <- saiqEventnames

Dsa <- Dsubj #start master sports activities table with subject-level descriptors

# =========================================================================
# Loop over activities ----------------------------------------------------
# =========================================================================
for ( code in activityCode[-length(activityCode)] ) { #skip final 'none' activity
  act <- activitiesTable[code,'Field']

  print(sprintf("== %s ==", act))

  p12Col <- paste0('sports_activity_', act, '_p12')
  tspentCol <- paste0('sports_activity_',act,'_tspent')
  perwkCol <- paste0('sports_activity_',act,"_","perwk")
  nmonthCol <- paste0('sports_activity_',act,"_nmonth")
  nyrCol <- paste0('sports_activity_',act,"_nyr")
  anyCol <- paste0('sports_activity_',act,"_any")
  asCol <- paste0('sports_activity_',act,'_ageStarted')

  # merge baseline and longitudinal
  # =====================================================================

  actVars <- paste0('sports_activity_',act,"_",measurements) #the final variables we'll use
  actVars_b <- paste0(actVars,'_p')
  actVars_l <- paste0(actVars,'_p_l')
  for (i in setdiff(1:length(measurements), baseOnlyIdx) ) {
    Dsa[,actVars[i]] <- as.character(D[,actVars_b[i]]) #start with baseline, fill in non-NA values from longitudinal
    longIdx = !is.na(D[,actVars_l[i]])
    Dsa[longIdx,actVars[i]] <- as.character(D[longIdx,actVars_l[i]])   #results as characters, not factors
  }

  # longitudinal surveys lack two variables: _nyr and _p12. Create them
  # =====================================================================

  #add to _p12 vector in later years from binary activities endorsements, don't include 'No activity', natch
  # =====================================================================
  Dsa[,  p12Col] <- D[,paste0(p12Col,'_p')] #start with baseline
  resp <- D[,paste0('sports_activity_activities_p_l___', code)]
  endorsed = (resp != 'not endorsed' & !is.na(resp))
  notEndorsed = (resp == 'not endorsed' & !is.na(resp))
  Dsa[endorsed,p12Col] <- 'Yes'
  Dsa[notEndorsed,p12Col] <- 'No'

  # convert categorical vars into numerical; add aggregate total hours
  # =====================================================================

  ## activity contexts: convert Yes/No to 1/0
  ynVars <- paste0('sports_activity_',act,'_',c("school","outside","private","self","p12"))
  Dsa[,ynVars] <- apply(Dsa[,ynVars], 2, yn2tf)

  ## add a  variable that is true if activity is endorsed in _any_ context
  #adata[paste0(act,"_any")] <- apply(adata[,paste0(act,"_",c("school","outside","private","self"))], 1, function (x) as.factor(ifelse(any(x=="Yes", na.rm=TRUE),"Yes",ifelse(any(x=="No", na.rm=TRUE),"No",NA))))
  # without usual na.rm, preserves FALSE meaning they endorsed it, but  all contexts are "No", which is bogus, vs did not endorse
  Dsa[,anyCol] <- apply(Dsa[,paste0('sports_activity_',act,"_",c("school","outside","private","self"))], 1, any)

  ## === tspent ===
  # minutes per session - convert from code to actual minutes
  # some assumptions here at extremes; <30 minutes --> 15; >3 hours --> 240 (4 hours)
  # _l has additional choice for 0 in couple of rows!? make it NA
  tmp <- Dsa[,tspentCol]
  tmp[tmp=="less than 30 minutes"] <- '15' #NB: assumption
  tmp[tmp=="60 (1 hr)"] <- '60'
  tmp[tmp=="90 (1.5 hrs)"] <- '90'
  tmp[tmp=="120 (2 hrs)"] <- '120'
  tmp[tmp=="150 (2.5 hrs)"] <- '150'
  tmp[tmp=="180 (3 hrs)"] <- '180'
  tmp[tmp=="greater than 3 hours"] <- '240' #NB: assumption
  Dsa[,tspentCol] <- tmp
  Dsa[,tspentCol] = handleDK(Dsa, tspentCol, doImputeDK)
  Dsa[which(Dsa[,tspentCol]==0), tspentCol] <- NA

  # === perwk: days per week ===
  tmp <- Dsa[,perwkCol]
  tmp[tmp=="Once every 2 weeks"] <- 0.5 #Once every 2 weeks
  tmp[tmp=="One day every month"] <- 0.25 #One day a month (once every 4 weeks)
  tmp[tmp=="Less than one day per month"] <- 0.1 #less than one day a month (arbitrary, small number akin to once every 2.5 months)
  Dsa[,perwkCol] <- tmp
  Dsa[,perwkCol] = handleDK(Dsa, perwkCol, doImputeDK)

  ## === nmonth ===
  # in longitudinal it can be "More than 12 months" - in that case, several options:
  # a) use age diff (months since last timepoint), b) just call it 12, as an estimate of months/year vs. months since last timepoint
  #   go with b), and have _nyr reflect true # years, so nmonth is just a proportionality for subset of year
  # Note, this will be changing in year 5, when additional choices will be added to the questionnaire
  gt12 <- which(Dsa[,nmonthCol] == 'More than 12 months')
  Dsa[gt12,nmonthCol] <- '12'
  Dsa[,nmonthCol] = handleDK(Dsa, nmonthCol, doImputeDK)
  Dsa[which(Dsa[,nmonthCol]==0), nmonthCol] <- NA #one row has value 0

  ## === nyr ===
  # convert baseline nyr to numeric
  Dsa[,nyrCol] = handleDK(D, paste0(nyrCol,'_p'), doImputeDK)

  #create longitudinal _nyr by incrementing previous timepoint's value (if any) by time passed in months if they endorsed activity that year
  # also update lifetimehrs with current year's total hours
  # NB: there are a handful of participants who missed the 1_year_follow_up...if they did music at baseline, compare to baseline
  # if not, we don't know when they started music!
  nyr <- Dsa[,c('src_subject_id','eventname','age',p12Col, nyrCol)]

  for (i in 2:length(saiqEventnames)) {
    prev <- nyr[tpidx[[i-1]], ]
    prev$index <- which(tpidx[[i-1]]) #include index into original table
    endorsed = tpidx[[i]] & (nyr[, p12Col]==1) #endorsed in this year
    now  <- nyr[endorsed, ]
    now$index <- which(endorsed)
    t <- right_join(prev,now,by='src_subject_id',suffix=c('.prev','.now')) #contains all those endorsing this year and their match from previous year
    prevNyr <- paste0(nyrCol,'.prev')
    thisNyr <- paste0(nyrCol,'.now')

    #fix up case when previous timepoint is unavailable.
    # NB this only works in v 3.0, and may fail when future followups are added. assumes only one possible skipped TP
    noPrev <- is.na(t[,'age.prev'])
    if (sum(noPrev)) {
      noPrevSubj <- t[noPrev,'src_subject_id']
      prev2 <- nyr[tpidx[[i-2]], ]
      prev2$index <- which(tpidx[[i-2]])
      prev2 <- prev2[prev2[,'src_subject_id'] %in% noPrevSubj,]
      tmp <- left_join(t,prev2,by='src_subject_id')
      t[noPrev,'age.prev'] <- tmp[noPrev,'age']
      t[noPrev,prevNyr] <- tmp[noPrev,nyrCol]
      t[noPrev,'index.prev'] <- tmp[noPrev,'index']
      t[noPrev,paste0(p12Col,'.prev')] <- tmp[noPrev,p12Col]
    }
    ageDiff <- t[,'age.now'] - t[,'age.prev'] #elapsed time between timepoints
    t[is.na(t[,prevNyr]), prevNyr] <- 0 #convert any NAs in previous nyr to 0 so addition works (i.e. people who started this year)
    t[which(t[,prevNyr]==999), prevNyr] <- 0 #convert any 'don't know' values to 1--it's the minimum that must be true
    t[, thisNyr] = t[, prevNyr] + ageDiff/12
    #merge nyr back into the main table.
    Dsa[endorsed,nyrCol] <- t[, thisNyr]
    # If this is first endorsed TP, initialize previous timepoint to _p12 FALSE and _nyr 0
    notYetStartedIdx <- t[is.na(Dsa[t[,'index.prev'], nyrCol]), 'index.prev']
    if (length(notYetStartedIdx)) {
      print(sprintf('%d Started %s at timepoint %d', length(notYetStartedIdx), act, i))
      Dsa[notYetStartedIdx, p12Col] <- FALSE
      Dsa[notYetStartedIdx, anyCol] <- FALSE
      Dsa[notYetStartedIdx, nyrCol] <- 0
      Dsa[notYetStartedIdx, tspentCol] <- 0
      Dsa[notYetStartedIdx, perwkCol] <- 0
      Dsa[notYetStartedIdx, nmonthCol] <- 0
    }
  }

  ## === ageStarted ===
  # add an approximate 'age started' variable
  # NB for dance, e.g., some 10 gave nyrs response suggesting they began < 1 year old, and 3 that they began before birth.
  # Question: are these completely bogus, or can we assume these are kids who have been doing it for a long time and the parents overshot a little?

  as <-  Dsa$ageYrs - Dsa[,nyrCol]
  if (sum(as<1,na.rm=TRUE))
    print(sprintf("ANOMALOUS: %s < {1, 0} years in {%d, %d} respondents",asCol, sum(as<1,na.rm=TRUE), sum(as<0,na.rm=TRUE)))
  #as[as<0] <- NA # Leave data as is, but report and remember to filter during analysis
  Dsa[,asCol] <- as

  ## === aggregate intensity ===
  # create  aggregate intensity measures: mean hours per week (across the year), lifetime total hours (assumes constant hours/week)
  # NB: This assumes nmonth is months/year _not_ total number of months since last timepoint (see above)

  #hrperwk
  # tspent*perwk*(nmonth/12) / 60
  Dsa[,paste0('sports_activity_',act,"_hrperwk")] <- Dsa[,tspentCol] * Dsa[,perwkCol] * (Dsa[,nmonthCol]/12) / 60

  #hrlifetime
  # tspent*perwk*4*nmonth*nyr / 60
  Dsa[,paste0('sports_activity_',act,"_hrlifetime")] <- Dsa[,tspentCol] * Dsa[,perwkCol] * 4 * Dsa[,nmonthCol] * Dsa[,nyrCol] / 60

} #loop on activities

# =========================================================================
# Music Listen / Reading --------------------------------------------------
# =========================================================================

# Create lmusic and read 'pseudo' activities -----------------------------
#=========================================================================

# music leisure listening and reading have a reduced set of measures, but we'll convert into full activities by assuming they are done in context 'self', 12 months/year, 'hours' is tspent*perwk,
# with no way to know how it's divided up, so just use the hrperwk value.
# TODO
musicListenVars <- c("sports_activity_lmusic_p","sports_activity_lmusic_years_p","sports_activity_lmusic_hours_p",
                    "sports_activity_lmusic_p_l",                                "sports_activity_lmusic_hours_p_l",
                    "scrn_hr_music")

readVars <- c("sports_activity_read_p","sports_activity_read_years_p","sports_activity_read_years_dk_p",
              "sports_activity_read_hours_p","sports_activity_read_hours_dk_p")

#= lmusic and read for later
#Dmr = D[, c(subjVars(1:2), usicListenVars, readVars)]

# =========================================================================
# Add aggregate participation 'activities': all Sports, all Arts ----------
# =========================================================================

print("== Adding aggregate sports & arts ==")

sports <- c("base","basket","climb","fhock","fball","gym","ihock","polo","iskate","m_arts","lax","rugby","skate","sboard","soc","surf","wpolo","tennis","run","mma","vball","yoga")
Dsa$sports_any      <- apply(Dsa[,paste0('sports_activity_',sports,"_any")], 1, any,na.rm=TRUE) #Note: apply is necessary here to force row-wise calculations
Dsa$sports_p12      <- apply(Dsa[,paste0('sports_activity_',sports,"_p12")], 1, any,na.rm=TRUE)
Dsa$sports_school   <- apply(Dsa[,paste0('sports_activity_',sports,"_school")], 1, sum,na.rm=TRUE)
Dsa$sports_outside  <- apply(Dsa[,paste0('sports_activity_',sports,"_outside")], 1, sum,na.rm=TRUE)
Dsa$sports_private  <- apply(Dsa[,paste0('sports_activity_',sports,"_private")], 1, sum,na.rm=TRUE)
Dsa$sports_self     <- apply(Dsa[,paste0('sports_activity_',sports,"_self")], 1, sum,na.rm=TRUE)
Dsa$sports_nyr      <- apply(Dsa[,paste0('sports_activity_',sports,"_nyr")], 1, mean,na.rm=TRUE)
Dsa$sports_nmonth   <- apply(Dsa[,paste0('sports_activity_',sports,"_nmonth")], 1, mean,na.rm=TRUE)
Dsa$sports_perwk    <- apply(Dsa[,paste0('sports_activity_',sports,"_perwk")], 1, mean,na.rm=TRUE)
Dsa$sports_tspent   <- apply(Dsa[,paste0('sports_activity_',sports,"_tspent")], 1, mean,na.rm=TRUE)
Dsa$sports_hrperwk  <- apply(Dsa[,paste0('sports_activity_',sports,"_hrperwk")], 1, sum,na.rm=TRUE)
Dsa$sports_hrlifetime <- apply(Dsa[,paste0('sports_activity_',sports,"_hrlifetime")], 1, sum,na.rm=TRUE)

arts <- c("dance","music", "art", "drama", "crafts")
Dsa$arts_any        <- apply(Dsa[,paste0('sports_activity_',arts,"_any")], 1, any,na.rm=TRUE)
Dsa$arts_p12        <- apply(Dsa[,paste0('sports_activity_',arts,"_p12")], 1, any,na.rm=TRUE)
Dsa$arts_school     <- apply(Dsa[,paste0('sports_activity_',arts,"_school")], 1, sum,na.rm=TRUE)
Dsa$arts_outside    <- apply(Dsa[,paste0('sports_activity_',arts,"_outside")], 1, sum,na.rm=TRUE)
Dsa$arts_private    <- apply(Dsa[,paste0('sports_activity_',arts,"_private")], 1, sum,na.rm=TRUE)
Dsa$arts_self       <- apply(Dsa[,paste0('sports_activity_',arts,"_self")], 1, sum,na.rm=TRUE)
Dsa$arts_nyr        <- apply(Dsa[,paste0('sports_activity_',arts,"_nyr")], 1, mean,na.rm=TRUE)
Dsa$arts_nmonth     <- apply(Dsa[,paste0('sports_activity_',arts,"_nmonth")], 1, mean,na.rm=TRUE)
Dsa$arts_perwk      <- apply(Dsa[,paste0('sports_activity_',arts,"_perwk")], 1, mean,na.rm=TRUE)
Dsa$arts_tspent     <- apply(Dsa[,paste0('sports_activity_',arts,"_tspent")], 1, mean,na.rm=TRUE)
Dsa$arts_hrperwk    <- apply(Dsa[,paste0('sports_activity_',arts,"_hrperwk")], 1, sum,na.rm=TRUE)
Dsa$arts_hrlifetime <- apply(Dsa[,paste0('sports_activity_',arts,"_hrlifetime")], 1, sum,na.rm=TRUE)

# indicate those doing music but not sports
Dsa$musicNoSport_any <- Dsa$sports_activity_music_any>0 & !Dsa$sports_any>0
Dsa$musicNoSport_p12 <- Dsa$sports_activity_music_p12 & !Dsa$sports_p12
# TODO: make variable for 'arts besides music'

# =========================================================================
# Musical Instruments -----------------------------------------------------
# =========================================================================

#observations (e.g. table(Dinst$sports_activity_music_instr_list))
# there are ~300 ocurrences of some of the most popular instruments, but the vast majority of responses were n/a!?
#  what can that mean, other than this data is disappointingly useless?
Dinst = D[, subjVars[1:2]]

# reformat intrument endorsements into subj x instrument binary indicator columns & #instruments columns & list column
instruments <- c('guitar','flute','woodwind','violin/viola','cello/bass','piano','harp','drums','horn','vocal','keyboard','dj','other','n/a')
instrumentCode <- c('0','1','2','3','4','5','6','7','8','9','10','11','12','801')
names(instruments) <- instrumentCode
# lookup using instruments[as.character(val)]

#transform binary indicators for each inst into a single new variable
# NB: we don't distinguish non-endorsement from NA
sports_activity_music_N_instr <- rep(0, nrow(D))
sports_activity_music_instr_list <- rep('',nrow(D))

for (code in instrumentCode) {
  oldCol = paste0('sports_activity_music_instr_p___',code)
  newCol = instruments[code]
  resp = D[,oldCol]
  endorsed = (resp != 'not endorsed' & !is.na(resp))
  newResp = rep(0,nrow(D))
  newResp[endorsed] <- 1
  Dinst[,newCol] <- newResp
  #increment number of instruments
  sports_activity_music_N_instr[endorsed] <- sports_activity_music_N_instr[endorsed] + 1
  #append intrument name to each list, taking care of commas
  len = lapply(sports_activity_music_instr_list[endorsed], function(x) {sapply(x, function(y) nchar(y))})
  comma = rep('',sum(endorsed))
  comma[len>0] <- ', '
  sports_activity_music_instr_list[endorsed] = paste0(sports_activity_music_instr_list[endorsed], comma, instruments[code])
}
Dinst = cbind(Dinst,sports_activity_music_N_instr,sports_activity_music_instr_list)

## Prune timepoints
goodTP <- grepl(saiqEventnames[1], Dsa[,'eventname']) | grepl(saiqEventnames[2], Dsa[,'eventname']) | grepl(saiqEventnames[3], Dsa[,'eventname'])
Dsa <- Dsa[goodTP,]
Dsa <- droplevels(Dsa)

#========================================================================= ==  =
# save -------------------------------------------------------------------------\/\/\/\/\
#========================================================================= ==  =

print(sprintf("== Saving: %s", outFileAct))
saveRDS(Dsa, file=outFileAct)

#save some convenience variables
#print(sprintf("== Saving: %s", outFileMeta))
#save(subjVars,activities,measurements,tstamp,file=outFileMeta)

#save long-form (in activities) table, with a column for each measure
# Note, need to be careful to update the column exclusion if adding more non-activity vars
DsaL <- Dsa %>%
  gather("activity_measure", "Value", -src_subject_id:-rsfmri_cor_network.gordon_auditory_network.gordon_visual ) %>%
  separate(activity_measure, c("activity", "measure"), sep='_', remove=TRUE) %>%
  spread(measure, Value)
DsaL <- select(DsaL, match(c(subjVars,"activity","any",measurements,"hrperwk","hrlifetime"),names(DsaL)))

print(sprintf("== Saving: %s", outFileLong))
saveRDS(DsaL, file=outFileLong)

# CSV of just music vars
print(sprintf("== Saving: %s", outFileMusic))
act = "music"
actVars <- names(Dsa)[grepl('_music_', names(Dsa))]
adata <- Dsa[,actVars]
musicData <- cbind(Dsubj, adata)
fwrite(musicData,file = outFileMusic)

print("Done.")

#close the log
sink()
