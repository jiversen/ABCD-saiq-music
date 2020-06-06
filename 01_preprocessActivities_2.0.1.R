
# Step 1: from raw NDA*.Rds to activities data
# Preprocess ABCD Activities questionnaire from DEAP 2.0.1 Rds file (doi:10.15154/1504431).
#   Subset, omitting columns of no (present) interest
#   Simplify variable naming
#   Convert activity amounts to numeric quantities and create aggregate 'intensity' measure
#   Create aggregate 'all sports' and 'all arts' activities
#
# Source: nda*.Rds
# Product: nda*_activities.Rds


# Settings ----------------------------------------------------------------

# I/O
if (Sys.info()["nodename"]  == "amusing.ucsd.edu") {
  ndaFile <- "C:/Users/jiversen/Documents/RDS/results/ABCD_releases_2.0.1_Rds/nda2.0.1.Rds"
  outFile <- "C:/Users/jiversen/Documents/RDS/results/nda2.0.1_activities.Rds"
  outFileMeta <- "C:/Users/jiversen/Documents/RDS/results/nda2.0.1_activities_meta.RData"
} else if (Sys.info()["nodename"] == "taiko.local") {
  ndaFile <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/RDS/results/ABCD_releases_2.0.1_Rds/nda2.0.1.Rds"
  outFile <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/RDS/results/nda2.0.1_activities.Rds"
  outFileMeta <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/RDS/results/nda2.0.1_activities_meta.RData"
}

# Analysis parameters
onlyBaseline = FALSE   # restrict to baseline timepoint

# Per-subject descriptive/demographic variables and covariates
subjVars <- c("subjectid","eventname","site_id_l","age", "sex","household.income","high.educ","anthro_bmi_calc","race_ethnicity","accult_phenx_q1","accult_phenx_q1_p")

# music leisure listening and reading have a reduced set of measures, but we'll convert into full activities by assuming they are done in context 'self', 12 months/year, 'hours' is tspent*perwk,
# with no way to know how it's divided up, so just use the hrperwk value.

musicListenVars <- c("sports_activity_lmusic_p","sports_activity_lmusic_years_p","sports_activity_lmusic_dk_p",
                    "sports_activity_lmusic_hours_p","sports_activity_lmusic_hours_dk_p","scrn_hr_music")

readVars <- c("sports_activity_read_p","sports_activity_read_years_p","sports_activity_read_years_dk_p",
              "sports_activity_read_hours_p","sports_activity_read_hours_dk_p")

# Activity variables (for each activity)
measurements <- c("school",	"outside",	"private",	"self",	"nyr",	"nmonth",	"perwk",	"tspent",	"p12")


# Preamble ----------------------------------------------------------------

#rm(list=ls())

library(tibble)
library(dplyr)
library(gsubfn)

#library(tidyr)
#library(data.table)
#library(rlang)
#library(corrplot)

# Load and subset ---------------------------------------------------------
D <- readRDS(ndaFile)

#select baseline timepoint
if (onlyBaseline) {
  D = D[D$eventname == "baseline_year_1_arm_1", ]
}

#make a few sub-tables: demographic
Dsubj = D[,subjVars]
#add age in years
Dsubj = add_column(Dsubj, ageYrs = Dsubj$age / 12, .after="age")
#rename language vars
names(D)[names(D)=="accult_phenx_q1"] = "english_skill_self"
names(D)[names(D)=="accult_phenx_q1_p"] = "english_skill_parent"

#lmusic and read for later
Dmr = D[, c(musicListenVars, readVars)]

#select columns of interest
saiqCols <- grepl("^sports_activity_",names(D)) & !grepl("_ss_",names(D)) & !grepl("_l$",names(D)) & !grepl("_activities_p_",names(D)) & !grepl("_select_language_",names(D)) & !names(D) %in% c(musicListenVars,readVars)
D = D[,saiqCols]

#clean up saiq field names
names(D) = sub("sports_activity_","",names(D)) #remove the prefix from SAIQ
names(D) = sub("_p$","",names(D)) #remove any _p suffix *NB--be sure this does not create clash with youth version--it would for accult_phenx, for ex. OK for SAIQ

names(Dmr) = sub("sports_activity_","",names(Dmr)) #remove the prefix from SAIQ
names(Dmr) = sub("_p$","",names(Dmr))

#fix some oddities in SAIQ -- doubled up fields for skate and sboard
D = add_column(D, skate_p12 = as.numeric(apply(data[c( "skate_p12_p___0",  "skate_p12_p___1")],1,function (x) any(as.numeric(as.character(x))==1, na.rm=TRUE))), .before="skate_p12_p___0")
D = add_column(D, sboard_p12 = as.numeric(apply(data[c( "sboard_p12_p___0",  "sboard_p12_p___1")],1,function (x) any(as.numeric(as.character(x))==1, na.rm=TRUE))), .before="sboard_p12_p___0")
D <- D[,!names(D) %in% c("skate_p12_p___0",  "skate_p12_p___1", "sboard_p12_p___0", "sboard_p12_p___1")]

#rename m_arts without underscore
D <- setnames(D,old=paste0('m_arts_', measurements), new=paste0('marts_',measurements))

activities <- unlist(strapplyc(names(D),"(.*)_school_p$"))

#Loop over the activities, calculating additional useful variables per-activity, and building the actData table
#Optionally, make a multi-panel plot with distributions of age started, hours/wk

actData <- Dsubj

for (act in activities) {
  actVars <- paste0(act,"_",measurements)
  adata <- D[,actVars]

  #convert factors to number
  adata[,] = sapply(adata[,], function (x) as.numeric(as.character(x)))

  #helper variable that is true if activity is endorsed in any context
  adata[paste0(act,"_any")] <- apply(adata[,paste0(act,"_",c("school","outside","private","self"))], 1, function (x) any(x==1, na.rm=TRUE))

  imputeDkToMean <- function (data, varname) {
    tmp = data[,varname]
    n999 = sum(tmp==999, na.rm=TRUE)
    mm = mean(tmp[tmp!=999], na.rm=TRUE)
    tmp[tmp==999] <- mm
    print(sprintf("Imputing %d %s to mean (%.2f)", n999, varname, mm))
    return(tmp)
  }

  #fix up codes
  #nyr
  var = paste0(act,"_nyr")
  tmp = adata[,var]
  # optionaly replace any 999 (don't know) with mean of given answers; otherwise, NA
  if (doImpute) {
    adata[,var] = imputeDkToMean(adata, var)
  } else {
    adata[,var][adata[,var]==999] <- NA #don't know
  }

  #add an approximate 'age started' variable
  adata[,paste0(act,'_ageStarted')] = data$ageYrs - adata[,nyr]

  #nmonth
  var = paste0(act,"_nmonth")
  tmp = adata[,var]
  # optionaly replace any 999 (don't know) with mean of given answers; otherwise, NA
  if (doImpute) {
    adata[,var] = imputeDkToMean(adata, var)
  } else {
    adata[,var][adata[,var]==999] <- NA #don't know
  }

  #convert perwk and tspent to real numbers
  # perwk: days per week
  perwk <- paste0(act,"_","perwk")
  tmp <- adata[,perwk]
  tmp[tmp==8] <- 0.5 #Once every 2 weeks
  tmp[tmp==9] <- 0.25 #One day a month (once every 4 weeks)
  tmp[tmp==10] <- 0.1 #less than one day a month (arbitrary, small number akin to once every 2.5 months)
  tmp[tmp==0] <- NA
  adata[,perwk] <- tmp
  if (doImpute) {
    adata[,perwk] <- imputeDkToMean(adata, perwk)
  } else {
    adata[,perwk][adata[,perwk]==999] <- NA #don't know
  }

  # tspent: minutes per session - convert from code to actual minutes
  tspent_minutes = c(NA, 15, 30, 45, 60, 90, 120, 150, 180, 240) #conversion from answercode+1 to minutes (from SAIQP_ABCD_Instrument.pdf)
  # some assumptions here at extremes; <30 minutes --> 15; >3 hours --> 240 (4 hours)
  tspent <- paste0(act,"_","tspent")
  tmp <- adata[,tspent]
  i999 = (tmp==999)
  tmp[i999] = NA #don't know
  tspent_ok = !is.na(tmp) #could simplify this and test for NA or 999
  tmp[tspent_ok] = tspent_minutes[tmp[tspent_ok]+1]
  adata[,tspent] <- tmp
  if (doImpute) { #have to do imputation after converting from code
    adata[,tspent] <- imputeDkToMean(adata, tspent)
  }

  #create  aggregate intensity measures: mean hours per week (across the year), lifetime total hours (assuming constant hours/week)
  # tspent*perwk*(nmonth/12) / 60
  adata[,paste0(act,"_hrperwk")] <- adata[paste0(act,"_tspent")] * adata[paste0(act,"_perwk")] * (adata[paste0(act,"_nmonth")]/12) / 60
  # tspent*perwk*4*nmonth*nyr / 60
  adata[,paste0(act,"_hrlifetime")] <- adata[paste0(act,"_tspent")] * adata[paste0(act,"_perwk")] * 4 * adata[paste0(act,"_nmonth")] * adata[paste0(act,"_nyr")] / 60

  #concatenate this expanded activity data onto the tabler
  actData <- cbind(actData, adata)

} #loop over activities

# Create lmusic and read 'pseudo' activities
# music leisure listening and reading have a reduced set of measures, but we'll convert into full activities by assuming they are done in context 'self', 12 months/year, 'hours' is tspent*perwk,
# with no way to know how it's divided up, so just use the hrperwk value.

# Add aggregate participation 'activities': all Sports, all Arts
sports <- c("base","basket","climb","fhock","fball","gym","ihock","polo","iskate","marts","lax","rugby","skate","sboard","soc","surf","wpolo","tennis","run","mma","vball","yoga")
actData$sports_any <-  apply(actData[,paste0(sports,"_any")], 1, sum,na.rm=TRUE)
actData$sports_p12 <- apply(actData[,paste0(sports,"_p12")], 1, sum,na.rm=TRUE)
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

actData$musicNoSport <- actData$music_any>0 & !actData$sports_any>0
actData$musicNoSport_p12 <- actData$music_p12 & !actData$sports_p12

#Now save it
timestamp = Sys.time()
saveRDS(actData, file=outFile)

#save some convenience variables
save(subjVars,activities,measurements,timestamp,file=outFileMeta)

########## FINISH

# Make a long-form (in activities) table, with a column for each measure

actDataL <- actData %>%
  gather("activity_measure", "Value", -subjectid:-scrn_hr_music ) %>%
  separate(activity_measure, c("activity", "measure"), sep='_', remove=TRUE) %>%
  spread(measure, Value)
actDataL <- select(actDataL, match(c(subjVars,"activity","any",measurements,"hrperwk","hrlifetime"),names(actDataL)))



# some summaries by activities
a <- actDataL %>% group_by(sex,activity) %>%
  summarise(lifetime = mean(hrlifetime,na.rm=TRUE)) %>%
  spread(sex, lifetime)

# Plot participation in activities
# simple distribution of participants endorsing any type of a given activity and in past 12 months
```{r}
percAct <- 100*sapply(actData[paste0(activities,"_any")],sum) / nrow(actData)
percp12 <- 100*sapply(actData[paste0(activities,"_p12")],sum,na.rm=TRUE) / nrow(actData)

percSport <- 100*sum(actData$sports_any,na.rm=TRUE) / nrow(actData)
percSport_p12 <- 100*sum(actData$sports_p12,na.rm=TRUE) / nrow(actData)
percArts <- 100*sum(actData$arts_any,na.rm=TRUE) / nrow(actData)
percArts_p12 <- 100*sum(actData$arts_p12,na.rm=TRUE) / nrow(actData)
#percMusicNoSport <- 100*sum(actData$musicNoSport,na.rm=TRUE) / nrow(actData)
#percMusicNoSport_p12 <- 100*sum(actData$musicNoSport_p12,na.rm=TRUE) / nrow(actData)
aggPerc <- as.data.frame(cbind(percArts, percArts_p12, percSport, percSport_p12))

print("=== Activity endorsement: p12")
sort(percp12,decreasing = TRUE)
par(mar=c(11,4,4,4));barplot(sort(percp12,decreasing = TRUE),las=2)

print("=== Activity endorsement: Lifetime")
sort(percAct,decreasing = TRUE)
par(mar=c(11,4,4,4));barplot(sort(percAct,decreasing = TRUE),las=2)
```
How many other activies musicians are involved in & does this differ from other activities?
```{r}
m <- data.matrix(actData[actData$music_p12==1 & !is.na(actData$music_p12), paste0(activities,"_p12")])
otherActivities = rowSums(m,na.rm=TRUE) - 1
par(mar=c(11,4,4,4));hist(otherActivities, breaks=0:20, freq=FALSE)

means=list()
for (act in activities) {
  col = paste0(act,"_p12");
  m <- data.matrix(actData[actData[col]==1 & !is.na(actData[col]), paste0(activities,"_p12")])
  otherActivities = rowSums(m,na.rm=TRUE) - 1
  par(mar=c(11,4,4,4));hist(otherActivities, breaks=0:26, freq=FALSE,main=col)
  means[act] = mean(otherActivities, na.rm=TRUE)
}
means = as.numeric(means)
names(means)=activities

par(mar=c(11,4,4,4));barplot(sort(means,decreasing = TRUE),las=2)

```



Activity participation by site
```{r}
for (site in levels(actData$abcd_site)) {
  thisSite = actData$abcd_site==site
  percp12 <- 100*sapply(actData[thisSite, paste0(activities,"_p12")],sum,na.rm=TRUE) / sum(thisSite)
  par(mar=c(11,4,4,4));barplot(sort(percp12,decreasing = TRUE),las=2,main=site)
}
```

Plot correlation between activities, hierarchically clustered
# Plot activities correlation
```{r}
#analyze correlations across different activities
mat <- data.matrix(actData[paste0(activities,"_p12")])
#mat <- data.matrix(actData[paste0(activities,"_any")])

mat[is.na(mat)] = 0
actCor <- cor(mat)
actCor <- actCor - diag(nrow(actCor))
df = 11000
t = actCor / sqrt( (1 - actCor^2) / (11000) )
p.value = 1 - pt(abs(t),df)
#emphasize colors (limits don't work properly on corrplot--cl.lim should give fullscale color, but instead only censors)
actCor <-  actCor * 2
lim <- 1
actCor[actCor > lim] = lim
actCor[actCor < -lim] = -lim


#corrplot(actCor, method="square",order="alphabet",diag=FALSE,cl.lim=c(-lim, lim))
corrplot(actCor, method="square",order="hclust",diag=FALSE,addrect=10,cl.lim=c(-lim, lim),p.mat = p.value, sig.level=0.05, insig="blank",tl.cex=.7)
```

Show similarity in a network form
https://psych-networks.com/r-tutorial-identify-communities-items-networks/
```{r}
library(qgraph)
cc=cor_auto(mat)
#corrplot(cc, method="square",order="hclust",diag=FALSE,addrect=6,p.mat = p.value, sig.level=0.05, insig="blank",tl.cex=.7)

graph<-qgraph(cc, graph="glasso", layout="spring", sampleSize = nrow(mat),
              vsize=7, cut=0, maximum=.45, border.width=1.5,threshold=TRUE)

library("igraph")
g = as.igraph(graph, attributes=TRUE)
sgc <- spinglass.community(g)
sgc$membership

#graph1<-qgraph(cc, graph="glasso", layout="spring", sampleSize = nrow(mat),
#              vsize=7, cut=0, maximum=.45, border.width=1.5,threshold=TRUE,
#              groups = sgc)


```

Do the correlation separately for each site
```{r}
for (site in levels(actData$abcd_site)) {
  mat <- data.matrix(actData[actData$abcd_site==site, paste0(activities,"_p12")])
  mat[is.na(mat)] = 0
  actCor <- cor(mat)
  #emphasize colors (limits don't work properly on corrplot--cl.lim should give fullscale color, but instead only censors)
  actCor <-  actCor * 2
  actCor[is.na(actCor)] = 0
  lim <- 1
  actCor[actCor > lim] = lim
  actCor[actCor < -lim] = -lim
  #corrplot(actCor, method="square",order="alphabet",diag=FALSE,cl.lim=c(-lim, lim))
  corrplot(actCor, method="square",order="hclust",diag=FALSE,addrect=10,cl.lim=c(-lim, lim),tl.cex=0.75)
}
```


#analyze location of different activities

#https://stackoverflow.com/questions/43624391/r-dplyr-methods-inside-own-function
summary.by <- function(data,var,group) {
  var <- enquo(var)
}

#% by region
regional <- actData %>% group_by(abcd_site) %>% mutate(siteN = n(), musicPerc = 100 * sum(music_p12,na.rm=TRUE)/n(), musicN = sum(music_p12,na.rm=TRUE))
print('=== participation by region ===')
print(arrange(aggregate(cbind(siteN,musicN, musicPerc) ~ abcd_site, data=regional, FUN=mean), desc(musicPerc)), row.names=FALSE)

#by gender
gender <- actData %>% group_by(sex) %>% mutate(siteN = n(), musicPerc = 100 * sum(music_p12,na.rm=TRUE)/n(), musicN = sum(music_p12,na.rm=TRUE)) #needed?
gender <- gender %>% group_by(sex) %>% summarise(siteN = n(), musicPerc = 100 * sum(music_p12,na.rm=TRUE)/n(), musicN = sum(music_p12,na.rm=TRUE))

#remove non-binary
#gender <- filter(gender, sex)
print('=== participation by gender ===')
print(arrange(aggregate(cbind(gender$siteN,gender$musicN, gender$musicPerc) ~ sex, data=gender, FUN=mean), row.names=FALSE))

# participation.summary <- function(activity, data) {
#   fn <- paste0("sai_p_",activity,"_",c("school","outside","private","self"))
#
#   return(any(data[2,fn]))
# }
#
# unlist(lapply(activities, participation.summary(x,data)))

#setnames(mdata,old=c("school","outside"),new=c("Group_school","Group_outside"))



#save for aggregation-------------------------------------------------------
shortData <- mdata[,c("subjectid", "anyArt", "p12")]
setnames(shortData, old=c("anyArt", "p12"), new=c(theArt, paste0(theArt,"_p12")))
if (artIdx==1) {
  allData <- shortData
} else {
  allData = full_join(allData, shortData)
}

# Descriptive analysis-------------------------------------------------------

print(paste("======== ", theArt, " ========"))

#%music overall
mdata3 <- mdata %>% mutate(percArt = 100 * sum(anyArt,na.rm=TRUE)/n(), percArtP12= 100 * sum(p12,na.rm=TRUE)/n(), artN = sum(anyArt,na.rm=TRUE), siteN = n())
print('=== participation ===')
print(paste("N=",mdata3[1,"siteN"], ", artN=", mdata3[1,"artN"] , ", percent=", mdata3[1,"percArt"], ", active=", mdata3[1,"percArtP12"]))

# music in past 12 months (assumes music at some point in life--question only asked if they indiate some music)
table(mdata[,"p12"],exclude = NULL)
#   0    1 <NA>
# 255 1656 2613
# implies 1656+255 = 1911 have some music ---42% of total 4524

# relationships between four types of music activity
table(mdata[,c("Group_school","Group_outside")])
table(mdata[,c("private","self")])

table(mdata[,c("Group_school","Group_outside","private","self")])

cor(mdata[,c("Group_school","Group_outside","private","self")], use = "complete.obs")

#mean practice amounts
onlymdata <- subset(mdata,anyArt==TRUE)
print('=== mean practice amounts ===')
print(summary(onlymdata[,c("nyr","nmonth","perwk","tspent")]))

#my.f = function(x) c(mean = mean(x,na.rm=TRUE), median = median(x,na.rm=TRUE))
#onlymdata[, sapply(.SD, my.f), .SDcols = c("nyr","nmonth","perwk","tspent"), by = site_name]


#music % by region
mdata2 <- mdata %>% group_by(abcd_site) %>% mutate(percArt = 100 * sum(anyArt,na.rm=TRUE)/n(), artN = sum(anyArt,na.rm=TRUE), siteN = n())
print('=== participation by region ===')
print(arrange(aggregate(cbind(siteN,artN, percArt) ~ abcd_site, data=mdata2, FUN=mean), desc(percArt)), row.names=FALSE)


# summary of demographics
mdata$sex = factor(mdata$sex)
print(summary(mdata[,c("ageYrs", "sex")]))

#annalyze aggregation
if (FALSE) {
  allData$anyArt = allData$music | allData$dance | allData$art | allData$drama | allData$crafts
  allData$anyArt_p12 = allData$music_p12 | allData$dance_p12 | allData$art_p12 | allData$drama_p12 | allData$crafts_p12
  allData$nArt = rowSums(allData[,arts], na.rm=TRUE)
  allData$nArt_p12 = rowSums(allData[,paste0(arts,"_p12")], na.rm=TRUE)

  print(table(allData[,"nArt"],exclude = NULL))
  print(table(allData[,"nArt_p12"],exclude = NULL))

  print(paste("N art=",sum(allData$anyArt, na.rm=TRUE), ", N art p12=", sum(allData$anyArt_p12, na.rm=TRUE)))


}

```
