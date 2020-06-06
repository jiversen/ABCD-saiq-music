# ABCD Activities questionnaire data

library(dtplyr)
library(dplyr)
library(tidyr)
library(data.table)
library(gsubfn)
library(rlang)
library(corrplot)

# load data ---------------------------------------------------------------
activitiesFile <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/Sports and Activities NDA17 Release data.csv"
data <- read.csv(activitiesFile)
names(data) = sub("sai_p_","",names(data)) #remove the prefix

#fix some oddities (unique to the export from frank?)
data$skate_p12  <- apply(data[c( "skate_p12___0",  "skate_p12___1")],1,any,na.rm=TRUE)
data$sboard_p12 <- apply(data[c("sboard_p12___0", "sboard_p12___1")],1,any,na.rm=TRUE)
data <- select(data,-c("skate_p12___0",  "skate_p12___1", "sboard_p12___0", "sboard_p12___1"))

# define standard fields
subjVars <- c("id_redcap","site_name","asnt_timestamp","tlfb_age_calc_inmonths_l","demo_gender_id_v2_l","hr_score","select_language___1")
measurements <- c("school",	"outside",	"private",	"self",	"nyr",	"nmonth",	"perwk",	"tspent",	"p12")

#rename m_arts without underscore
data <- setnames(data,old=paste0('m_arts_', measurements), new=paste0('marts_',measurements))

# get unique activities
activities <- unlist(strapplyc(colnames(data),"(.*)_school$"))

#  per-activity computation---------------------------------------------------------------
actData = data[,subjVars]
for (act in activities) {
  actVars <- paste0(act,"_",measurements)
  adata <- data[,actVars]
  adata[paste0(act,"_any")] <- apply(adata[,paste0(act,"_",c("school","outside","private","self"))], 1, any,na.rm=TRUE)

  #fix up codes
  #convert perwk and tspent to real numbers
  # days per week
  perwk <- paste0(act,"_","perwk")
  tmp <- adata[,perwk]
  tmp[tmp==8] <- 0.5 #Once every 2 weeks
  tmp[tmp==9] <- 0.25 #One day a month (once every 4 weeks)
  tmp[tmp==10] <- 0.1 #less than one day a month (arbitrary, small number akin to once every 2.5 months)
  tmp[tmp==0] <- NA
  adata[,perwk] <- tmp

  # minutes per session - convert from code to actual minutes
  tspent_minutes = c(NA, 15,30,45, 60, 90, 120, 150, 180, 240) #conversion from answercode+1 to minutes (from SAIQP_ABCD_Instrument.pdf)
  tspent <- paste0(act,"_","tspent")
  tmp <- adata[,tspent]
  tspent_ok = !is.na(tmp)
  tmp[tspent_ok] = tspent_minutes[tmp[tspent_ok]+1]
  adata[,tspent] <- tmp

  #create  aggregate intensity measures: mean hours per week, lifetime hours
  #tspent*perwk*(nmonth/12) / 60
  adata[paste0(act,"_hrperwk")] <- adata[paste0(act,"_tspent")] * adata[paste0(act,"_perwk")] * (adata[paste0(act,"_nmonth")]/12) / 60

  adata[paste0(act,"_hrlifetime")] <- adata[paste0(act,"_tspent")] * adata[paste0(act,"_perwk")] * 4 * adata[paste0(act,"_nmonth")] * adata[paste0(act,"_nyr")] / 60


  #concatenate it
  actData <- cbind(actData, adata)

}

#--------- add aggregated participation numbers: sports, arts, music&!sport

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


#actData$musicNoSport <- actData$music_any>0 & !actData$sports_any>0
#actData$musicNoSport_p12 <- actData$music_p12 & !actData$sports_p12

# --------- make a long-form (in activities) table, with a column for each measure
actDataL <- actData %>%
  gather("activity_measure", "Value", -id_redcap:-select_language___1 ) %>%
  separate(activity_measure, c("activity", "measure"), sep='_', remove=TRUE) %>%
  spread(measure, Value)
actDataL <- select(actDataL, match(c(subjVars,"activity","any",measurements,"hrperwk","hrlifetime"),names(actDataL)))

# some summaries by activities
a <- actDataL %>% group_by(demo_gender_id_v2_l,activity) %>%
  summarise(lifetime = mean(hrlifetime,na.rm=TRUE)) %>%
  spread(demo_gender_id_v2_l, lifetime)

#simple distribution of participants endorsing any type of a given activity and in past 12 months

percAct <- 100*sapply(actData[paste0(activities,"_any")],sum) / nrow(actData)
percp12 <- 100*sapply(actData[paste0(activities,"_p12")],sum,na.rm=TRUE) / nrow(actData)

percSport <- 100*sum(actData$sports_any,na.rm=TRUE) / nrow(actData)
percSport_p12 <- 100*sum(actData$sports_p12,na.rm=TRUE) / nrow(actData)
percArts <- 100*sum(actData$arts_any,na.rm=TRUE) / nrow(actData)
percArts_p12 <- 100*sum(actData$arts_p12,na.rm=TRUE) / nrow(actData)
#percMusicNoSport <- 100*sum(actData$musicNoSport,na.rm=TRUE) / nrow(actData)
#percMusicNoSport_p12 <- 100*sum(actData$musicNoSport_p12,na.rm=TRUE) / nrow(actData)
aggPerc <- as.data.frame(cbind(percArts, percArts_p12, percSport, percSport_p12))

par(mar=c(11,4,4,4));barplot(sort(percp12,decreasing = TRUE),las=2)

#analyze correlations across different activities
mat <- data.matrix(actData[paste0(activities,"_p12")])
mat[is.na(mat)] = 0
actCor <- cor(mat)
actCor <- actCor - diag(nrow(actCor))
actCor <-  actCor * 2
lim <- 1
actCor(actCor < -lim) = -lim
actCor(actCor > lim) = lim


corrplot(actCor, method="square",order="alphabet",diag=FALSE,cl.lim=c(-lim, lim))
corrplot(actCor, method="square",order="hclust",diag=FALSE,addrect=10,cl.lim=c(-lim, lim))




#analyze location of different activities

#https://stackoverflow.com/questions/43624391/r-dplyr-methods-inside-own-function
summary.by <- function(data,var,group) {
  var <- enquo(var)
}

#% by region
regional <- actData %>% group_by(site_name) %>% mutate(siteN = n(), musicPerc = 100 * sum(music,na.rm=TRUE)/n(), musicN = sum(music,na.rm=TRUE), wpoloPerc=100*sum(wpolo,na.rm=TRUE)/n(),wpoloN=sum(wpolo,na.rm=TRUE))
print('=== participation by region ===')
print(arrange(aggregate(cbind(siteN,musicN, musicPerc,wpoloN,wpoloPerc) ~ site_name, data=regional, FUN=mean), desc(wpoloPerc)), row.names=FALSE)

#by gender
gender = filter(actData, demo_gender_id_v2_l<=2)
gender <- gender %>% group_by(demo_gender_id_v2_l) %>% mutate(siteN = n(), musicPerc = 100 * sum(music,na.rm=TRUE)/n(), musicN = sum(music,na.rm=TRUE))

gender <- gender %>% group_by(demo_gender_id_v2_l) %>% summarise(siteN = n(), musicPerc = 100 * sum(music,na.rm=TRUE)/n(), musicN = sum(music,na.rm=TRUE))

#remove non-binary
gender <- filter(gender, demo_gender_id_v2_l<=2)
print('=== participation by gender ===')
print(arrange(aggregate(cbind(siteN,musicN, musicPerc) ~ demo_gender_id_v2_l, data=gender, FUN=mean), row.names=FALSE))

# participation.summary <- function(activity, data) {
#   fn <- paste0("sai_p_",activity,"_",c("school","outside","private","self"))
#
#   return(any(data[2,fn]))
# }
#
# unlist(lapply(activities, participation.summary(x,data)))

#setnames(mdata,old=c("school","outside"),new=c("Group_school","Group_outside"))



#save for aggregation-------------------------------------------------------
shortData <- mdata[,c("id_redcap", "anyArt", "p12")]
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
mdata2 <- mdata %>% group_by(site_name) %>% mutate(percArt = 100 * sum(anyArt,na.rm=TRUE)/n(), artN = sum(anyArt,na.rm=TRUE), siteN = n())
print('=== participation by region ===')
print(arrange(aggregate(cbind(siteN,artN, percArt) ~ site_name, data=mdata2, FUN=mean), desc(percArt)), row.names=FALSE)


# summary of demographics
mdata$demo_gender_id_v2_l = factor(mdata$demo_gender_id_v2_l)
print(summary(mdata[,c("tlfb_age_calc_inmonths_l", "demo_gender_id_v2_l")]))

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

