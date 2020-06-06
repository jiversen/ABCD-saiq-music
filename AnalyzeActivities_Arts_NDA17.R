# ABCD Activities questionnaire data
library(dtplyr)
library(dplyr)
library(tidyr)
library(data.table)

# load data ---------------------------------------------------------------
activitiesFile <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/Sports and Activities NDA17 Release data.csv"

subjFields <- c("id_redcap","site_name","asnt_timestamp","tlfb_age_calc_inmonths_l","demo_gender_id_v2_l","hr_score","select_language___1")

artIdx <- 5
arts <- c("music", "dance", "art", "drama", "crafts")
theArt = arts[artIdx]
artFieldsBase <- c("_school",	"_outside",	"_private",	"_self",	"_nyr",	"_nmonth",	"_perwk",	"_tspent",	"_p12")
artFields <- paste0(theArt, artFieldsBase)

data = read.csv(activitiesFile)
names(data) = sub("sai_p_","",names(data)) #remove the prefix

mdata <- data[,c(subjFields, artFields)]

#make names generic for rest of code
names(mdata) = sub(paste0(theArt,"_"), "", names(mdata)) #remove art-specific prefix

setnames(mdata,old=c("school","outside"),new=c("Group_school","Group_outside"))

mdata$anyArt = mdata$Group_school | mdata$Group_outside | mdata$private | mdata$self
#mdata["anyArt"][is.na(mdata["anyArt"])] <- FALSE

#convert perwk and tspent to real numbers
# days per week
mdata$perwk[mdata$perwk==8] <- 0.5 #Once every 2 weeks
mdata$perwk[mdata$perwk==9] <- 0.25 #One day a month (once every 4 weeks)
mdata$perwk[mdata$perwk==10] <- 0.1 #less than one day a month (arbitrary, small number akin to once every 2.5 months)
mdata$perwk[mdata$perwk==0] <- NA

# minutes per session - convert from code to actual minutes
tspent_minutes = c(NA, 15,30,45, 60, 90, 120, 150, 180, 240) #conversion from answercode+1 to minutes (from SAIQP_ABCD_Instrument.pdf)
tspent_ok = !is.na(mdata$tspent)
mdata$tspent[tspent_ok] = tspent_minutes[mdata$tspent[tspent_ok]+1]

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

