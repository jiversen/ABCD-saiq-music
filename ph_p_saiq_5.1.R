# ABCD 5.1 ph_p_saiq table
# http://dx.doi.org/10.15154/z563‑zd24


# this is a master table-maker as it also includes detailed demographics, PRS, PCS
# Changelist
#   12/2  Update to ABCD 5.1, Add PRS and PCS

#

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Settings ----------------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

rm(list=ls())

tstamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
scriptName <- "ph_p_saiq_5.1.R"

# we are only concerned with yearly assessments
saiqEventnames <- c('baseline_year_1_arm_1','1_year_follow_up_y_arm_1','2_year_follow_up_y_arm_1','3_year_follow_up_y_arm_1','4_year_follow_up_y_arm_1')
keyCols = c("src_subject_id","eventname")

# Analysis parameters
doImputeDK = TRUE
doLog = TRUE
doDebug = FALSE
useDeapDemos = TRUE

options(tibble.print_max=100, tibble.print_min=150, tibble.width=400)

# DEBUGGING
if (doDebug) {
  opt.orig = options()
  options(warn=2) #for Debugging, converts warnings into errors
  #options(error=recover)
} else {
  options(warn=1)
  #options(error=NULL)
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Lib -------------------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(tidyverse)
library(readr)
library(reshape2)
library(patchwork)
library(insight)
library(data.table)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Setup Files -------------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

source('./setupPaths_5.1.R')
resultDir = file.path(resultDirRoot,tstamp)
outFileBase  <- file.path(resultDir, "abcd_5.1_activities")
outFileAct   <- paste0(outFileBase, '.Rds')
outFileInst  <- paste0(outFileBase, '_inst.Rds')
outFileLong  <- paste0(outFileBase, '_long.Rds')
outFileMeta  <- paste0(outFileBase, '_meta.RData')
outFileMusic <- paste0(outFileBase, '_music.csv')
outFileSubj  <- paste0(outFileBase, '_subj.Rds')

inFile <- file.path(releaseDir, "physical-health/ph_p_saiq.csv")

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Start Logging -----------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if (!doDebug){
  dir.create(resultDir,recursive=TRUE,mode="0775")

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

  writeLines(sprintf("\nParameters:\n\tdoImputeDK=%s\n\n",doImputeDK))
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Load --------------------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

print_color("Loading Activities data...","bblue")
D <- read_csv(inFile, guess_max = 25000 ) #auto-detect makes a lot of mistakes for 'rare' data
# everything except src_subject_id, eventname should be numeric. A few oddballs come through as characters depending on exact value of guess_max, so just be aware
D <- as.data.frame(lapply(D, as.factor)) #5.x now seems to use only numeric values for all factors
# activities have 48728 timepoints
print_color("Done.\n","blue")

## Add Demographics  ==============================================================

if (0 & !useDeapDemos) {
  # prepend subject info (my initial way)
  source("./abcd_y_lt_5.1.R") #this has 49151 yearly timepoints

  #== remove non-year follow-ups, which have no answers for SAIQ
  abcd_y_lt <- abcd_y_lt %>% filter(!grepl('_month_', eventname)) %>% droplevels()

  subjCols = setdiff(names(abcd_y_lt), keyCols)

  #demographics is a superset of datapoints available in sai, so expand rows of sai to match (will have some empty rows)
  D = full_join(abcd_y_lt, D, by=c("src_subject_id","eventname"), relationship="one-to-one") #unmatched="error",
  #sanity check, should be no rows in D that do not have a match in abcd_y_lt, so cannot have more rows than we started with. (but) D will not have every timepoint in it
  if (nrow(D) != nrow(abcd_y_lt)) stop('Error joining saiq to subject info')


} else {
  print_color("Prepending DEAP Demographics...\n","blue")
  # do a more complete job with full demographics
  source(file.path(toolsDir, "cmig_tools_utils/r/makeDEAPdemos.R"))
  source(file.path(toolsDir, "cmig_tools_utils/r/makeDesign.R"))
  datapath <- file.path(releaseDir, "abcd-general/")
  deapdemos <- makeDEAPdemos(datapath) #this has 48807 yearly timepoints
  #== add age in years
  deapdemos = add_column(deapdemos, ageYrs = deapdemos$interview_age / 12, .after="interview_age")

  # add genetic ancestry PCs
  print_color(sprintf("Adding genetic ancestry PC1-10 from %s...\n", pcsFile), "bblue")
  pcs = read.table(pcsFile, header=TRUE)
  colnames(pcs)[colnames(pcs)=='IID'] = 'src_subject_id'
  pcs <- pcs[,c('src_subject_id', paste0("PC",1:10))] #take first 10 PCs
  deapdemos = join(deapdemos, pcs, by='src_subject_id')

  # add PRS
  print_color(sprintf("Adding PRS from %s...\n", prsFile), "bblue")
  prs = read.table(prsFile, header=TRUE)
  prs <- prs %>% rename_with(~ paste0("prs_", .x))
  colnames(prs)[colnames(prs)=='prs_ID'] = 'src_subject_id'

  deapdemos = join(deapdemos, prs, by='src_subject_id')

  D = full_join(deapdemos, D, by=c("src_subject_id","eventname"), relationship="one-to-one") #unmatched="error",
  # Join has 48808 rows--must be one in activities that is not found in demos!
  # these rows have no interview_age 11091 11092 11093 11094 48808
}

#== there is one bad subject, 'NDAR_INV749XW1TD', that does not have interview_date and interview_age, remove for now after confirming the problem
if (any(is.na(D[D$src_subject_id=='NDAR_INV749XW1TD', 'interview_age'][[1]]))) {
  print_color(sprintf("NDAR_INV749XW1TD has no interview_age or demographics: removing\n"),'red')
  D = D[D$src_subject_id!='NDAR_INV749XW1TD',]
}
#== also NDAR_INVMZG6CUGJ 4_year_follow_up_y_arm_1
missingAge = which(is.na(D$interview_age))
print_color(sprintf("Removing additional timepoints with no interview_age or demographics: %s\n",paste0(D[missingAge,"src_subject_id"],", ",D[missingAge,"eventname"])),'red')
D = D[-missingAge,]

# double check we haven't missed any others
if (any(is.na(D[, 'interview_age'][[1]]))) {
  error("Some additional subjects are missing an age")
}

# fill in missing demographic variables
demoVars = setdiff(names(deapdemos), keyCols)
D <- D %>% group_by(src_subject_id) %>%
  fill(all_of(demoVars), .direction = "downup")




# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Some basic descriptions of the data and what we'll do with it  ---------
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ==events
unique(D$eventname) #baseline, 1, 2, 3, 4 year follow-up
# [1] "baseline_year_1_arm_1"    "1_year_follow_up_y_arm_1" "2_year_follow_up_y_arm_1" "3_year_follow_up_y_arm_1" "4_year_follow_up_y_arm_1"

# ==music variables
musicVars <- colnames(D)[grepl("music",colnames(D))]
# [1] "sai_p_music_school"          "sai_p_music_outside"         "sai_p_music_private"         "sai_p_music_self"
# [5] "saip2_music_nyr"             "saip2_music_nmonth"          "saip2_music_perwk"           "saip2_music_tspent"
# [9] "sai_p_music_p12"             "sai_p_lmusic"                "sai_p_lmusic_2"              "sai_p_lmusic_dk"
# [13] "sai_p_lmusic_hours"          "sai_p_lmusic_hours_dk"       "sai_ss_music_nyr_p"          "sai_ss_music_nmonth_p"
# [17] "sai_ss_music_perwk_p"        "sai_ss_music_tspent_p"       "sai_ss_lmusic_years_p"       "sai_ss_lmusic_hours_p"
# [21] "sports_activity_ss_lmusic_p" "sai_yr3_lmusic_hrs_base_p"   "sai_yr3_lmusic_p"            "sai_yr3_lmusic_hrs_p12_p"
# [25] "sai_yr3_lmusic_yrs_p"        "sai_p_music_school_l"        "sai_p_music_outside_l"       "sai_p_music_private_l"
# [29] "sai_p_music_self_l"          "sai_p_music_nmonth_l"        "sai_p_music_perwk_l"         "sai_p_music_tspent_l"
# [33] "sai_p_lmusic_l"              "sai_p_lmusic_hours_l"        "sai_p_lmusic_hours_dk_l"     "sai_p_music_instr___7"
# [37] "sai_p_music_instr___8"       "sai_p_music_instr___9"       "sai_p_music_instr___10"      "sai_p_music_instr___11"
# [41] "sai_p_music_instr___12"      "sai_p_music_instr___801"     "sai_p_music_self_pract_l"    "sai_p_music_instr___0"
# [45] "sai_p_music_instr___1"       "sai_p_music_instr___2"       "sai_p_music_instr___3"       "sai_p_music_instr___4"
# [49] "sai_p_music_instr___5"       "sai_p_music_instr___6"       "sai_p_lmusic_hours_2_l"      "sai_ss_music_nmonth_p_l"
# [53] "sai_ss_music_perwk_p_l"      "sai_ss_music_tspent_p_l"     "sai_ss_lmusic_hours_p_l"     "sai_ss_lmusic_hours_2_p_l"

# all baseline data are in base variable names and all follow up (longitudinal) are in that variable suffixed with _l

# the saip2_ variables are only present for baesline--just a typo or something? Weird.
# shortcut to diagnose which events have values for which variables:
#   unique(D[!is.na(D[["saip2_music_nmonth"]]),"eventname"])

# what are the _ss_ vars?

# == Y3
#   there was a a special case of data collection in yr3--asking about a new set of activities...all obscure sporting and outdoors activities--curling, orienteering...no new arts
#   but included some listening and reading variables--asked of the parents? IGNORE -- the lmusic scene is a total mess. Let's focus on the youth questions in 3_ and 4_ year followups *phy_y_saiq*)

y3Vars <- colnames(D)[grepl("_yr3_",colnames(D))]
# [1] "sai_yr3_lmusic_hrs_base_p"     "sai_yr3_lmusic_p"              "sai_yr3_lmusic_hrs_p12_p"      "sai_yr3_lmusic_yrs_p"          "sai_yr3_p_select_language___1" "sai_yr3_read_hrs_base_p"       "sai_yr3_read_p"
# [8] "sai_yr3_read_hrs_p12_p"        "sai_yr3_read_yrs_p"

# colnames(D)[grepl("_yr_3_",colnames(D)))]
# [10] "sai_p_yr_3_activities___30"    "sai_p_yr_3_activities___31"    "sai_p_yr_3_activities___32"    "sai_p_yr_3_activities___33"    "sai_p_yr_3_activities___34"
# [15] "sai_p_yr_3_activities___35"    "sai_p_yr_3_activities___36"    "sai_p_yr_3_activities___37"    "sai_p_yr_3_activities___38"    "sai_p_yr_3_activities___39"    "sai_p_yr_3_activities___40"    "sai_p_yr_3_activities___41"
# [22] "sai_p_yr_3_activities___42"    "sai_p_yr_3_activities___43"    "sai_p_yr_3_activities___44"

# sai_p_activities___23(_l) is music

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Helper Functions --------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# handle "Don't know" answers in one of two ways: 1) set to NA, 2) replace with mean of valid answers
#  the logic here is DK is perhaps different than 'no answer', because we know they do an activity.
#   similarly, in case of endorsed music, sometimes the odd measure is NA, so also impute those
#  refinement: compute mean by timepoint
# NOTE: function assumes input is numeric, so must convert factors to numbers before calling (e.g. "60 (1 hr)" -> "60")

handleDK <- function(data, varname, endorsedCol, doImpute) {
  DK  <-  c(999)
  tmp  <-  data[[varname]]
  endorsed <- data[[endorsedCol]]
  if (!is.numeric(tmp))    stop("handleDK: Expected numeric values for column: ", varname)
  events  <-  unique(data[['eventname']])
  isImputed <- rep(FALSE, dim(data)[1])
  for (en in events) {
    isTp  <-  grepl(en, data[['eventname']])
    isDK  <-  (tmp %in% DK | (endorsed & is.na(tmp)))
    tpDK  <-  which(isDK & isTp)
    tpnDK  <-  which(!isDK & isTp & endorsed) #calc mean on all DK values for endorsed participants
    nDK  <-  length(tpDK)

    if (nDK) {
      print_color(sprintf(" DK:  %d (%.1f%%) in %s (@ %s)\n",nDK, 100*nDK/length(tpnDK), varname, en),'blue')
      if (doImpute) {
        mm  <-  mean(tmp[tpnDK], na.rm=TRUE)
        tmp[tpDK] <- mm
        print_color(sprintf("   Imputing DK to mean (%.2f)\n", mm),'bblue')
        isImputed[tpDK] <- TRUE
      } else {
        tmp[tpDK] <- NA
      }
    }
  }
  return(list(val=tmp, hasImputed=isImputed))
}


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# SAIQ-specific preprocessing ---------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

## fix oddities ===========================================================

#  doubled up fields for skate and sboard - ___0 is all the No's and ___1 all the Yes's with 'not endorsed' in the rest. Is there a difference between 'no' and 'not endorsed'? I think not, but preserve as NA
sports_activity_skate_p12_p <-  rep(NA,nrow(D))
sports_activity_skate_p12_p[D$sai_p_skate_p12___0=="0"] = "0"
sports_activity_skate_p12_p[D$sai_p_skate_p12___1=="1"] = "1"
D = add_column(D, sai_p_skate_p12 = as.factor(sports_activity_skate_p12_p), .before="sai_p_skate_p12___1")

sports_activity_sboard_p12_p <-  rep(NA,nrow(D))
sports_activity_sboard_p12_p[D$sai_p_sboard_p12___0=="0"] = "0"
sports_activity_sboard_p12_p[D$sai_p_sboard_p12___1=="1"] = "1"
D = add_column(D, sai_p_sboard_p12 = as.factor(sports_activity_sboard_p12_p), .before="sai_p_sboard_p12___1")

D <- D[,!names(D) %in% c("sai_p_skate_p12___0",  "sai_p_skate_p12___1", "sai_p_sboard_p12___0", "sai_p_sboard_p12___1")]

## Activity Variables =====================================================

# the same set of variables is present for each activity
measurements <- c("school","outside","private","self","nyr","nmonth","perwk","tspent","p12")
# NB nyr, and p12 present only at baseline, we will synthesize in later years: nyr = nyr_priortimepoint+endorsed; p12 = endorsed
baseOnlyMeasurementsIdx <- c(5,9)
binaryMeasurementsIdx <- c(1,2,3,4,9)

# activities conversion table
activityName <- c('Dance','Baseball','Basketball','Climbing','Field Hockey','Football','Gymnastics','Ice Hockey','Horseback','Skating','Martial Arts','Lacrosse','Rugby','Skateboard',
                  'Snowsports','Soccer','Surfing','Swimming','Tennis','Running','Wrestling','Volleyball','Yoga','Music','Visual Art','Drama','Crafts','Games','Collecting','None' )
activityField <- c('dance','base','basket','climb','fhock','fball','gym','ihock','polo','iskate','m_arts','lax','rugby','skate','sboard','soc','surf','wpolo','tennis','run',
                   'mma','vball','yoga','music','art','drama','crafts','chess','collect','?None?')
activityCode <- as.character(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)) #seq(0,29), but explicit because verified in dictionary for sai_p_activities___# binaries

activitiesTable <- data.frame(Activity = activityName, Field = activityField, stringsAsFactors=FALSE)
rownames(activitiesTable) <- activityCode #means must look up using code as a string to find the row name

activityCodeToProcess = activityCode[-length(activityCode)] #skip final 'none' activity

# helper to look up activity name from variable name
act.name <- function(field) {
  return ( activitiesTable$Activity[activitiesTable$Field == field] )
}

# *DEBUG* #######################################################################
#debug on music only
debugSubjs <- c('NDAR_INV00X2TBWJ','NDAR_INV00HEV6HB')

if (doDebug) {
  activityCodeToProcess = c("15", "22", "23")
  print_color('DEBUG: Test only on activity 23 "music" and subset of participants\n','red')
  debugSubjs <- c('NDAR_INV02JXJUZG', 'NDAR_INV019DXLU4', 'NDAR_INV003RTV85') # subjects with gaps in music endorsement that are not working
  debugSubjs <- c('NDAR_INV00X2TBWJ', 'NDAR_INV00HEV6HB', 'NDAR_INV02H7G2T6')
  debugSubjs <- c('NDAR_INV00X2TBWJ', 'NDAR_INV00BD7VDC')
  debugSubjs <- c('NDAR_INV00X2TBWJ','NDAR_INV00HEV6HB')
  #debugSubjs <- debugSubjs[c(1,2)]
  D0=D
  D = D0[D0[["src_subject_id"]] %in% debugSubjs,]
}
# ++++++++++++ #######################################################################


## timepoint indexing ====================================================


#create selection vectors for each timepoint, use to look up 'previous' timepoint's value for nyr
tpidx <- list()
en <- D[['eventname']]
for (i in 1:length(saiqEventnames)) {
  tpidx[i] <- list(grepl(saiqEventnames[i], en))
}
names(tpidx) <- saiqEventnames

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Build Activities Table --------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#start master table with keys and subject demographic info
Dsa = select(D, all_of(c(keyCols, demoVars)))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Main loop through activities to build activities table -----------------

for ( code in activityCodeToProcess ) {
  act <- activitiesTable[code,'Field']

  print_color(sprintf("\n\n== %s ==\n", act),'bold')

  ## normalized variable names ===========================================

  prefix = "sai_p_"
  p12Col <- paste0(prefix, act, '_p12')
  tspentCol <- paste0(prefix,act,'_tspent')
  perwkCol <- paste0(prefix,act,"_","perwk")
  nmonthCol <- paste0(prefix,act,"_nmonth")
  nyrCol <- paste0(prefix,act,"_nyr")
  anyCol <- paste0(prefix,act,"_any")
  asCol <- paste0(prefix,act,'_ageStarted')
  hrlifetimeCol <- paste0(prefix,act,"_hrlifetime")
  hrperyrCol <- paste0(prefix,act,"_hrperyr")
  hrperwkCol <- paste0(prefix,act,"_hrperwk")


  # final names
  actVars <- paste0(prefix, act, "_", measurements)

  # baseline names
  actVars_b <- actVars
  actVars_b[5:8] = paste0("saip2_", act, "_", measurements[5:8])


  # longitudinal names
  actVars_l <-paste0(prefix, act, "_", measurements, '_l')

  #synthesize an 'endorsed' measure
  endorsedCol <- paste0(prefix,act,'_endorsed')
  actVars <- append(actVars, endorsedCol)
  actVars_b <- append(actVars_b, paste0('sai_p_activities___', code))
  actVars_l <- append(actVars_l, paste0('sai_p_activities_l___', code))
  binaryMeasurementsIdx <- append(binaryMeasurementsIdx, length(actVars))

  # keep track of which rows have imputed values for Don't Knows
  imputedCol <- paste0(prefix,act,'_anyimputed')
  allimputedCol <- paste0(prefix,act,'_allimputed')
  hasImputed <- rep(FALSE, dim(Dsa)[1])
  allImputed <- rep(TRUE, dim(Dsa)[1])


  ## merge baseline and longitudinal ====================================

  for (i in setdiff(1:length(actVars), baseOnlyMeasurementsIdx) ) {
    Dsa[[actVars[i]]] <- D[[actVars_b[i]]] #start with baseline, fill in non-NA values from longitudinal
    longIdx = !is.na(D[[actVars_l[i]]])
    # longitudinal typically has extra factor for "0", which will be implicitly converted to NA if we do nothing, but could be others, too, like "13" months (?!)
    #   so, first expand the factor space. (An alternative is to do assignment as.character and convert back to factor)
    levels(Dsa[[actVars[i]]]) <- c(levels(Dsa[[actVars[i]]]), setdiff(levels(D[[actVars_l[i]]]), levels(Dsa[[actVars[i]]])))
    Dsa[longIdx,actVars[i]] <- D[longIdx,actVars_l[i]]
    #convert to logical if appropriate
    if (i %in% binaryMeasurementsIdx) {
      Dsa[[actVars[i]]] <- Dsa[[actVars[i]]] == "1"   # otherwise as.logical(as.numeric(as.character(Dsa[[actVars[i]])))
    }
  }

  # endorsed = NA -> FALSE
  Dsa[is.na(Dsa[[endorsedCol]]), endorsedCol] <- FALSE

  ## create missing longitudinal variables ==============================

  # longitudinal surveys lack two variables: _nyr and _p12. Create them and add a number of other convenience variables

  ### ==== p12 ====

  # generate  in later years from binary activities endorsements, don't include 'No activity', natch
  p12idx = which(grepl("p12",measurements))
  Dsa[[actVars[p12idx]]] <- D[[actVars_b[p12idx]]] == "1" #start with baseline, converting to logical

  # there are several NA at baseline, fix these
  p12blna = tpidx[[1]] & is.na(Dsa[[actVars[p12idx]]])
  if (sum(p12blna)) {
    print_color(sprintf("  Found %d records missing p12. Setting to Endorsed\n", sum(p12blna)),'red')
    Dsa[p12blna, p12Col] <- Dsa[p12blna, endorsedCol]
  }
  # fill in later time points where by definition p12=endorsed
  resp <- D[[paste0('sai_p_activities_l___', code)]] == "1" #longitudinal endorsements-
  endorsed = (resp == TRUE & !is.na(resp))
  Dsa[endorsed, actVars[p12idx]] <- TRUE

  ### ==== any ====

  # add a  variable that is true if activity is endorsed in _any_ context
  Dsa[[anyCol]] <- apply(Dsa[,paste0(prefix,act,"_",c("school","outside","private","self"))], 1, any, na.rm=TRUE)

  ### ==== tspent ====

  # minutes per session - convert from code to numerical minutes
  # some assumptions here at extremes; <30 minutes --> 15; >3 hours --> 240 (4 hours)
  # _l has additional choice for 0 in couple of rows!? make it NA
  # 999 = Don't know No lo sé ; 0 = 0; 1 = less than 30 minutes menos de 30 minutos ; 2 = 30; 3 = 45; 4 = 60 (1 hr) 60 (1 hora) ; 5 = 90 (1.5 hrs) 90 (1.5 horas) ; 6 = 120 (2 hrs) 20 (2 horas) ; 7 = 150 (2.5 hrs) 150 (2.5 horas) ; 8 = 180 (3 hrs) 180 (3 horas) ; 9 = greater than 3 hours Mas de 3 horas | (minutes) (minutos)
  tmp <- as.numeric(as.character(Dsa[[tspentCol]]))
  tmp[tmp==0] <- 0
  tmp[tmp==1] <- 15   #NB: an assumption "less than 30 minutes"
  tmp[tmp==2] <- 30
  tmp[tmp==3] <- 45
  tmp[tmp==4] <- 60
  tmp[tmp==5] <- 90
  tmp[tmp==6] <- 120
  tmp[tmp==7] <- 150
  tmp[tmp==8] <- 180
  tmp[tmp==9] <- 240 #NB: assumption for >3 hours (=4)
  Dsa[[tspentCol]] <- tmp

  res = handleDK(Dsa, tspentCol, endorsedCol, doImputeDK)
  if (any(res$hasImputed)) {
    Dsa[[tspentCol]] <- res$val
    hasImputed <- hasImputed | res$hasImputed
  }
  allImputed <- allImputed & res$hasImputed

  Dsa[Dsa[[tspentCol]]==0 & !is.na(Dsa[[tspentCol]]), tspentCol] <- NA

  ### ==== perwk: days per week ====

  # 0 = 0; 1 = 1; 2 = 2; 3 = 3; 4 = 4; 5 = 5; 6 = 6; 7 = 7; 8 = Once every 2 weeks; 9 = One day every month; 10 = Less than one day per month/; 999 = Don't know
  # DD notes that not answered = 0
  tmp <- as.numeric(as.character(Dsa[[perwkCol]]))
  tmp[tmp==8] <- 0.5 #Once every 2 weeks
  tmp[tmp==9] <- 0.25 #One day a month (once every 4 weeks)
  tmp[tmp==10] <- 0.1 #less than one day a month (arbitrary, small number akin to once every 2.5 months)
  Dsa[[perwkCol]] <- tmp

  res = handleDK(Dsa, perwkCol, endorsedCol, doImputeDK)
  if (any(res$hasImputed)) {
    Dsa[[perwkCol]] <- res$val
    hasImputed <- hasImputed | res$hasImputed
  }
  allImputed <- allImputed & res$hasImputed

  #Dsa[[perwkCol]] = replace(Dsa[[perwkCol]], Dsa[[perwkCol]]==0, NA) #alternative syntax, maybe more readable?
  Dsa[Dsa[[perwkCol]]==0 & !is.na(Dsa[[perwkCol]]), perwkCol] <- NA

  ### ==== nmonth ====
  # in longitudinal it can be "More than 12 months" - in that case, several options:
  # a) use age diff (months since last timepoint)
  # b) just call it 12, as an estimate of months/year vs. months since last timepoint
  #   go with b), and have _nyr reflect true # years, so nmonth is just a proportionality for subset of year
  # Note, this will be changing in year 5, when additional choices will be added to the questionnaire
  Dsa[[nmonthCol]] <- as.numeric(as.character(Dsa[[nmonthCol]]))
  gt12 <- which(Dsa[[nmonthCol]] == 13)
  Dsa[gt12,nmonthCol] <- 12

  res = handleDK(Dsa, nmonthCol, endorsedCol, doImputeDK)
  if (any(res$hasImputed)) {
    Dsa[[nmonthCol]] <- res$val
    hasImputed <- hasImputed | res$hasImputed
  }
  allImputed <- allImputed & res$hasImputed


  Dsa[Dsa[[nmonthCol]]==0 & !is.na(Dsa[[nmonthCol]]), nmonthCol] <- NA #one row has value 0

  ### ==== aggregate intensity ====
  # create  aggregate intensity measures: mean hours per week (across the year), lifetime total hours (assumes constant hours/week)
  # NB: This assumes nmonth is months/year _not_ total number of months since last timepoint (see above)

  #hrperwk
  # tspent*perwk*(nmonth/12) / 60
  Dsa[[hrperwkCol]] <- Dsa[[tspentCol]] * Dsa[[perwkCol]] * (Dsa[[nmonthCol]]/12) / 60
  Dsa[[hrperyrCol]] <- Dsa[[hrperwkCol]] * 52
  #initialize hrlifetime--copy over non-NA values from hrperyr
  Dsa[[hrlifetimeCol]] <- 0
  nonna = tpidx[[1]] & !is.na(Dsa[[hrperyrCol]])
  Dsa[nonna, hrlifetimeCol] <- Dsa[nonna, hrperyrCol]

  ### ==== nyr ====

  # convert baseline nyr to numeric
  nyridx = which(grepl("nyr",measurements))
  Dsa[[nyrCol]] = as.numeric(as.character(D[[actVars_b[nyridx]]]))

  #handle missing (NA) or don't know NYR for music endorsers at baseline
  DK  <-  c(999)
  meanNyr <-  mean(Dsa[tpidx[[1]] & Dsa[[nyrCol]] != DK & !is.na(Dsa[[nyrCol]]), nyrCol][[1]], na.rm=TRUE) #can do this because only baseline is non-NA
  NaNyr <- meanNyr # or = 1. #NB: this is the choice--impute to mean or to the minimum that must be true (1)
  # endorsed, any and p12
  # if p12 is TRUE and any is TRUE -> we know at least 1, or can impute the mean (~2)
  imputeNyr = tpidx[[1]] & Dsa[[endorsedCol]] & Dsa[[p12Col]] & !is.na(Dsa[[p12Col]]) & (is.na(Dsa[[nyrCol]]) | Dsa[[nyrCol]] == DK )
  Dsa[imputeNyr ,nyrCol] <- NaNyr
  #endorsed, any and ~p12
  # if p12 is FALSE, but any is true we can call it mean-1
  imputeNyrm1 = tpidx[[1]] & Dsa[[endorsedCol]] & !Dsa[[p12Col]] & !is.na(Dsa[[p12Col]]) & (is.na(Dsa[[nyrCol]]) | Dsa[[nyrCol]] == DK )
  Dsa[imputeNyrm1 ,nyrCol] <- NaNyr-1

  #finally, initialize all those not endorsing at baseline to have 0 nyr & hrlifetime
  Dsa[tpidx[[1]] & !Dsa[[endorsedCol]], nyrCol] <- 0
  Dsa[tpidx[[1]] & !Dsa[[endorsedCol]], hrlifetimeCol] <- 0


  #check
  if (sum(Dsa[[nyrCol]]==999, na.rm=T)) {
    error('Some remaining DK in nyr')
  }

  #  DECIDE on false endorsements #####################################----
  # We've finished all imputation
  Dsa[[imputedCol]] <- hasImputed
  Dsa[[allimputedCol]] <- allImputed

  # time for a reckoning. There are some cases where activity was endorsed, but responses were FALSE and DK and all performance measures were imputed. In that case, reasonable to assume it was a mistaken endorsement
  #   or unreliable at least, so let's call these non-activity timepoints
  bogus <- Dsa[[anyCol]] == FALSE & !is.na(Dsa[[anyCol]]) & Dsa[[allimputedCol]] == TRUE & !is.na(Dsa[[allimputedCol]])
  nBogus = sum(bogus, na.rm=T)
  if (nBogus) {
    print_color(sprintf("  *** Found %d bogus %s endorsements (no data entered). De-endorsing.\n", nBogus, act),'red')
    Dsa[bogus, endorsedCol] <- FALSE
    Dsa[bogus, p12Col] <- FALSE
    Dsa[bogus, c(nmonthCol, perwkCol, tspentCol, hrperwkCol, hrperyrCol)] <- NA
    Dsa[bogus & tpidx[[1]], nyrCol] <- 0 #only set baseline to 0
    Dsa[bogus & tpidx[[1]], hrlifetimeCol] <- 0 #only set baseline to 0
  }

  #a minority (~100 in case of music) of participants have NA for baseline nyr, despite endorsing it.
  #   Rather than lose these, ASSUME conservatively that nyr is 1 (which it must be--at least)
  #   For bogus endorsements, set it to 0, appropriately
  # missingBaselineNyr = (Dsa[[endorsedCol]] == TRUE & !is.na(Dsa[[endorsedCol]]) & is.na(Dsa[[nyrCol]]) & Dsa$eventname == 'baseline_year_1_arm_1')
  # bogusBaseline = bogus & Dsa$eventname == 'baseline_year_1_arm_1'
  # n = sum(missingBaselineNyr, na.rm=TRUE)
  # if (n) {
  #   print_color(sprintf('  Fixing %d missing values for %s at baseline\n', n, nyrCol),'bred')
  #   Dsa[missingBaselineNyr & !bogusBaseline, nyrCol] <- 1
  #   Dsa[missingBaselineNyr & bogusBaseline, nyrCol] <- 0
  # }


  #create longitudinal _nyr by incrementing previous timepoint's value (if any) by time passed in months if they endorsed activity that year
  # also update lifetimehrs with current year's total hours
  # NB: there are a handful of participants who missed the 1_year_follow_up...if they did music at baseline, compare to baseline
  # if not, we don't know when they started music!

  # loop through timepoints, looking back to calculate cumulative nyr of activity
  for (i in 2:length(saiqEventnames)) {
    if (!sum(tpidx[[i]])) break #nothing for this timepoint, nothing to do.
    print_color(sprintf('*Cumulating nyr at %s\n', saiqEventnames[i]),'green')
    nyr <- Dsa[,c('src_subject_id','eventname','ageYrs', nyrCol, hrlifetimeCol, hrperyrCol, endorsedCol, p12Col, anyCol, imputedCol)]
    prev <- nyr[tpidx[[i-1]], ]
    prev$index <- which(tpidx[[i-1]]) #include index into original table
    now <- nyr[tpidx[[i]], ]
    now$index <- which(tpidx[[i]]) # index into original table
    t <- right_join(prev,now,by='src_subject_id',suffix=c('.prev','.now')) #contains prior timepoint matching everyone at this timepoint

    #copy forward
    prevNyr <- paste0(nyrCol,'.prev')
    thisNyr <- paste0(nyrCol,'.now')
    t[[thisNyr]] <- t[[prevNyr]] #first step is to copy over last timepoint's nyr value since experience cannot go down. Below we'll bump up the nyr for those currently endorsing
    prevHrlifetime <- paste0(hrlifetimeCol,'.prev')
    thisHrlifetime <- paste0(hrlifetimeCol,'.now')
    t[[thisHrlifetime]] <- t[[prevHrlifetime]]
    t0 <- t

    if (0 & act=="music" & i==3) { #debug magic
      View(t[t$src_subject_id %in% debugSubjs,])
      browser()
    }

    # endorsement is now robust because bogus endorsements have been removed above
    endorsed.now <- (t[[paste0(endorsedCol,'.now')]]==TRUE & !is.na(t[[paste0(endorsedCol,'.now')]]))
    t <- t0[endorsed.now, ]
    tn <- t0[!endorsed.now, ]

    iEndorsed <- t[,"index.now"][[1]]
    inEndorsed <- tn[,"index.now"][[1]]


    #merge not-endorsed values back into main table
    #notendorsed <-  tpidx[[i]] & (nyr[[endorsedCol]]==FALSE) & !is.na(nyr[[endorsedCol]]) #endorsed in this year (indexed into full dataset) #strange and subtle bug here
    Dsa[inEndorsed,nyrCol] <- tn[[thisNyr]]
    Dsa[inEndorsed,hrlifetimeCol] <- tn[[thisHrlifetime]]

    #fix up case when previous timepoint was unavailable and there are prior timepoints -- to handle missing timepoints
    for (lookBack in seq(2, length(saiqEventnames)-1) ) {
      if (i - lookBack < 1) break #can only look back as far as we have data
      noPrev <- is.na(t[['ageYrs.prev']]) #if previous TP has no age, means it was missing, so look back
      if (sum(noPrev)) {
        print_color(sprintf("  %d have no prior (t-%d) timepoint\n", sum(noPrev), lookBack-1),'bgreen')
        noPrevSubj <- t[noPrev,'src_subject_id'][[1]]
        prev2 <- nyr[tpidx[[i-lookBack]],]
        prev2$index <- which(tpidx[[i-lookBack]])
        prev2 <- prev2[prev2[['src_subject_id']] %in% noPrevSubj,]
        tmp <- left_join(t,prev2,by='src_subject_id')
        t[noPrev,'ageYrs.prev'] <- tmp[noPrev,'ageYrs']
        t[noPrev,prevNyr] <- tmp[noPrev,nyrCol]
        t[noPrev,thisNyr] <- t[noPrev,prevNyr]
        t[noPrev,'index.prev'] <- tmp[noPrev,'index']
        t[noPrev,paste0(p12Col,'.prev')] <- tmp[noPrev,p12Col]
        t[noPrev ,'eventname.prev'] <- saiqEventnames[i-lookBack]
      }
    }

    #calculate cumulative number of years based on prior time point and years elapsed
    ageDiff <- t[['ageYrs.now']] - t[['ageYrs.prev']] #elapsed time between timepoints
    t[is.na(t[[prevNyr]]), prevNyr] <- 0 #convert any NAs in previous nyr to 0 so addition works (i.e. people who started this year)
    t[is.na(t[[prevHrlifetime]]), prevHrlifetime] <- 0 #convert any NAs in previous herlifetime to 0 so addition works (i.e. people who started this year)

    #on occasion, nyr at baseline may be DK. Handle these cases conservatively
    prevDK = t[[prevNyr]]==999
    if (sum(prevDK, na.rm=TRUE)) {
      print_color(sprintf("Found %d DK in nyr at %s\n", sum(prevDK, na.rm=TRUE), saiqEventnames[i-1]),'red')
      t[prevDK, prevNyr] <- 1 #convert any 'don't know' values to 1--it's the minimum that must be true if music was endorsed--expected to be quite rare
    }
    t[[thisNyr]] = t[[prevNyr]] + ageDiff

    thisHrperyr <- paste0(hrperyrCol,'.now')
    t[is.na(t[[thisHrperyr]]), thisHrperyr] <- 0 #convert any NAs to 0 so addition works
    t[[thisHrlifetime]] = t[[prevHrlifetime]] + t[[thisHrperyr]]

    #merge back into the main table.
    #endorsed <-  tpidx[[i]] & (nyr[[endorsedCol]]==TRUE) & !is.na(nyr[[endorsedCol]]) #endorsed in this year (indexed into full dataset) #seems to work, but use bulletproof indexes from table t
    Dsa[iEndorsed,nyrCol] <- t[[thisNyr]]
    Dsa[iEndorsed,hrlifetimeCol] <- t[[thisHrlifetime]]

    # If this is first endorsed TP, initialize previous timepoint to _p12 FALSE and _nyr 0
    #  approach, find all subjects doing music now with NA in the previous timepoint's nyr, i.e. who had not started previously
    notYetStartedTable <- t[is.na(Dsa[t[['index.prev']], hrperyrCol]), 'index.prev']
    notYetStartedIdx <- notYetStartedTable[['index.prev']]
    if (FALSE&length(notYetStartedIdx)) {
      print_color(sprintf('  %d Started %s at timepoint %d\n', length(notYetStartedIdx), act, i),'bgreen')
      Dsa[notYetStartedIdx, p12Col] <- FALSE
      Dsa[notYetStartedIdx, anyCol] <- FALSE
      Dsa[notYetStartedIdx, tspentCol] <- 0
      Dsa[notYetStartedIdx, perwkCol] <- 0
      Dsa[notYetStartedIdx, nmonthCol] <- 0
      Dsa[notYetStartedIdx, hrperwkCol] <- 0
      Dsa[notYetStartedIdx, hrperyrCol] <- 0
    }
  }

  ### ==== ageStarted ====
  # add an 'age started' variable. This is only valid at the first endorsed timepoint for each subject
  # NB for dance, e.g., some 10 gave nyrs response suggesting they began < 1 year old, and 3 that they began before birth.
  # Question: are these completely bogus, or can we assume these are kids who have been doing it for a long time and the parents overshot a little?

  as <-  Dsa$ageYrs - Dsa[[nyrCol]]
  if (sum(as<1, na.rm=TRUE))
    print_color(sprintf("ANOMALOUS: %s < {1, 0} years in {%d, %d} respondents\n",asCol, sum(as<1,na.rm=TRUE), sum(as<0,na.rm=TRUE)),'red')
  #as[as<0] <- NA # Leave data as is, but report and remember to filter during analysis
  Dsa[[asCol]] <- as
  for (subj in unique(Dsa[['src_subject_id']])) {
    subjIdx = which(Dsa[['src_subject_id']]==subj)
    subjTable <- Dsa[subjIdx,]
    endorsed=subjTable[[endorsedCol]]
    as <- subjTable[[asCol]]
    if (all(is.na(as))) break
    firstTrue = which(endorsed)[1]
    if (!is.na(firstTrue)) { #we found an endorsement
      as[-firstTrue] <- as[firstTrue] #could set all to NA so as to have just one value per subject
    } else {                 #no endorsement
      as[!is.na(as)] <- NA
    }
    Dsa[subjIdx, asCol] <- as
  }

} #loop over activities

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Add aggregate participation 'activities': all Sports, all Arts ----------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

print("== Adding aggregate sports & arts ==")

# all sports
sports <- c("base","basket","climb","fhock","fball","gym","ihock","polo","iskate","m_arts","lax","rugby","skate","sboard","soc","surf","wpolo","tennis","run","mma","vball","yoga")
Dsa$sports_any      <- apply(Dsa[,paste0(prefix,sports,"_any")], 1, any,na.rm=TRUE) #Note: apply is necessary here to force row-wise calculations
Dsa$sports_p12      <- apply(Dsa[,paste0(prefix,sports,"_p12")], 1, any,na.rm=TRUE)
Dsa$sports_endorsed <- apply(Dsa[,paste0(prefix,sports,"_endorsed")], 1, any,na.rm=TRUE)
Dsa$sports_school   <- apply(Dsa[,paste0(prefix,sports,"_school")], 1, sum,na.rm=TRUE)
Dsa$sports_outside  <- apply(Dsa[,paste0(prefix,sports,"_outside")], 1, sum,na.rm=TRUE)
Dsa$sports_private  <- apply(Dsa[,paste0(prefix,sports,"_private")], 1, sum,na.rm=TRUE)
Dsa$sports_self     <- apply(Dsa[,paste0(prefix,sports,"_self")], 1, sum,na.rm=TRUE)
Dsa$sports_nyr      <- apply(Dsa[,paste0(prefix,sports,"_nyr")], 1, mean,na.rm=TRUE)
Dsa$sports_nmonth   <- apply(Dsa[,paste0(prefix,sports,"_nmonth")], 1, mean,na.rm=TRUE)
Dsa$sports_perwk    <- apply(Dsa[,paste0(prefix,sports,"_perwk")], 1, mean,na.rm=TRUE)
Dsa$sports_tspent   <- apply(Dsa[,paste0(prefix,sports,"_tspent")], 1, mean,na.rm=TRUE)
Dsa$sports_hrperwk  <- apply(Dsa[,paste0(prefix,sports,"_hrperwk")], 1, sum,na.rm=TRUE)
Dsa$sports_hrperyr  <- apply(Dsa[,paste0(prefix,sports,"_hrperyr")], 1, sum,na.rm=TRUE)
Dsa$sports_hrlifetime <- apply(Dsa[,paste0(prefix,sports,"_hrlifetime")], 1, sum,na.rm=TRUE)
Dsa$sports_ageStarted <- apply(Dsa[,paste0(prefix,sports,"_ageStarted")], 1, min,na.rm=TRUE)

# all arts
arts <- c("dance","music", "art", "drama", "crafts")
Dsa$arts_any        <- apply(Dsa[,paste0(prefix,arts,"_any")], 1, any,na.rm=TRUE)
Dsa$arts_p12        <- apply(Dsa[,paste0(prefix,arts,"_p12")], 1, any,na.rm=TRUE)
Dsa$arts_endorsed   <- apply(Dsa[,paste0(prefix,arts,"_endorsed")], 1, any,na.rm=TRUE)
Dsa$arts_school     <- apply(Dsa[,paste0(prefix,arts,"_school")], 1, sum,na.rm=TRUE)
Dsa$arts_outside    <- apply(Dsa[,paste0(prefix,arts,"_outside")], 1, sum,na.rm=TRUE)
Dsa$arts_private    <- apply(Dsa[,paste0(prefix,arts,"_private")], 1, sum,na.rm=TRUE)
Dsa$arts_self       <- apply(Dsa[,paste0(prefix,arts,"_self")], 1, sum,na.rm=TRUE)
Dsa$arts_nyr        <- apply(Dsa[,paste0(prefix,arts,"_nyr")], 1, mean,na.rm=TRUE)
Dsa$arts_nmonth     <- apply(Dsa[,paste0(prefix,arts,"_nmonth")], 1, mean,na.rm=TRUE)
Dsa$arts_perwk      <- apply(Dsa[,paste0(prefix,arts,"_perwk")], 1, mean,na.rm=TRUE)
Dsa$arts_tspent     <- apply(Dsa[,paste0(prefix,arts,"_tspent")], 1, mean,na.rm=TRUE)
Dsa$arts_hrperwk    <- apply(Dsa[,paste0(prefix,arts,"_hrperwk")], 1, sum,na.rm=TRUE)
Dsa$arts_hrperyr    <- apply(Dsa[,paste0(prefix,arts,"_hrperyr")], 1, sum,na.rm=TRUE)
Dsa$arts_hrlifetime <- apply(Dsa[,paste0(prefix,arts,"_hrlifetime")], 1, sum,na.rm=TRUE)
Dsa$arts_ageStarted <- apply(Dsa[,paste0(prefix,arts,"_ageStarted")], 1, min,na.rm=TRUE)

# arts withut music; FIXME: however, this may still include people doing music
artsnm <- c("dance", "art", "drama", "crafts")
Dsa$artsnm_any        <- apply(Dsa[,paste0(prefix,artsnm,"_any")], 1, any,na.rm=TRUE)
Dsa$artsnm_p12        <- apply(Dsa[,paste0(prefix,artsnm,"_p12")], 1, any,na.rm=TRUE)
Dsa$artsnm_endorsed   <- apply(Dsa[,paste0(prefix,artsnm,"_endorsed")], 1, any,na.rm=TRUE)
Dsa$artsnm_school     <- apply(Dsa[,paste0(prefix,artsnm,"_school")], 1, sum,na.rm=TRUE)
Dsa$artsnm_outside    <- apply(Dsa[,paste0(prefix,artsnm,"_outside")], 1, sum,na.rm=TRUE)
Dsa$artsnm_private    <- apply(Dsa[,paste0(prefix,artsnm,"_private")], 1, sum,na.rm=TRUE)
Dsa$artsnm_self       <- apply(Dsa[,paste0(prefix,artsnm,"_self")], 1, sum,na.rm=TRUE)
Dsa$artsnm_nyr        <- apply(Dsa[,paste0(prefix,artsnm,"_nyr")], 1, mean,na.rm=TRUE)
Dsa$artsnm_nmonth     <- apply(Dsa[,paste0(prefix,artsnm,"_nmonth")], 1, mean,na.rm=TRUE)
Dsa$artsnm_perwk      <- apply(Dsa[,paste0(prefix,artsnm,"_perwk")], 1, mean,na.rm=TRUE)
Dsa$artsnm_tspent     <- apply(Dsa[,paste0(prefix,artsnm,"_tspent")], 1, mean,na.rm=TRUE)
Dsa$artsnm_hrperwk    <- apply(Dsa[,paste0(prefix,artsnm,"_hrperwk")], 1, sum,na.rm=TRUE)
Dsa$artsnm_hrperyr    <- apply(Dsa[,paste0(prefix,artsnm,"_hrperyr")], 1, sum,na.rm=TRUE)
Dsa$artsnm_hrlifetime <- apply(Dsa[,paste0(prefix,artsnm,"_hrlifetime")], 1, sum,na.rm=TRUE)
Dsa$artsnm_ageStarted <- apply(Dsa[,paste0(prefix,artsnm,"_ageStarted")], 1, min,na.rm=TRUE)


# indicate those doing music but not sports
Dsa$musicNoSport_any <- Dsa$sai_p_music_endorsed & !Dsa$sports_endorsed
Dsa$musicNoSport_p12 <- Dsa$sai_p_music_p12 & !Dsa$sports_p12

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Musical Instruments -----------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#observations (e.g. table(Dinst$sports_activity_music_instr_list))
# there are ~300 ocurrences of some of the most popular instruments, but the vast majority of responses were n/a!?
#  what can that mean, other than this data is disappointingly useless?
Dinst = D[, keyCols]

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
  oldCol = paste0('sai_p_music_instr___',code)
  newCol = instruments[code]
  resp = D[[oldCol]] == "1"
  endorsed = (resp == TRUE & !is.na(resp))
  newResp = rep(FALSE,nrow(D))
  newResp[endorsed] <- TRUE
  Dinst[,newCol] <- newResp
  #increment number of instruments
  sports_activity_music_N_instr[endorsed] <- sports_activity_music_N_instr[endorsed] + 1
  #append instrument name to each list, taking care of commas
  len = lapply(sports_activity_music_instr_list[endorsed], function(x) {sapply(x, function(y) nchar(y))})
  comma = rep('',sum(endorsed))
  comma[len>0] <- ', '
  sports_activity_music_instr_list[endorsed] = paste0(sports_activity_music_instr_list[endorsed], comma, instruments[code])
}

Dinst$sports_activity_music_N_instr <- sports_activity_music_N_instr
Dinst$sports_activity_music_instr_list <- sports_activity_music_instr_list

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> >>> >>
# Save -------------------------------------------------------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> >>> >>

if (!doDebug){
  print_color(sprintf("== Saving: %s", outFileAct),'green')
  Dsa <- droplevels(Dsa)
  saveRDS(Dsa, file=outFileAct)

  #save some convenience variables

  print_color(sprintf("== Saving: %s", outFileInst),'green')
  Dinst <- droplevels(Dinst)
  saveRDS(Dinst, file=outFileInst)


  #print(sprintf("== Saving: %s", outFileMeta))
  #save(subjVars,activities,measurements,tstamp,file=outFileMeta)

  #save long-form (in activities) table, with a column for each measure
  # Note, need to be careful to update the column exclusion if adding more non-activity vars
  # DsaL <- Dsa %>%
  #   gather("activity_measure", "Value", -src_subject_id:-rsfmri_cor_network.gordon_auditory_network.gordon_visual ) %>%
  #   separate(activity_measure, c("activity", "measure"), sep='_', remove=TRUE) %>%
  #   spread(measure, Value)
  # DsaL <- select(DsaL, match(c(subjVars,"activity","any",measurements,"hrperwk","hrlifetime"),names(DsaL)))
  #
  # print(sprintf("== Saving: %s", outFileLong))
  # saveRDS(DsaL, file=outFileLong)

  # CSV of just music vars
  print(sprintf("== Saving: %s", outFileMusic))
  act = "music"
  actVars <- names(Dsa)[grepl('_music_', names(Dsa))]
  musicData <- Dsa[,c(colnames(Dsa)[1:6], actVars)]
  fwrite(musicData,file = outFileMusic)

  print("Done.")

  #close the log
  sink()
}
