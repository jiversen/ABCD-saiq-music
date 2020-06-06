# ABCD Activities questionnaire data
library(dtplyr)
library(dplyr)
library(tidyr)
library(data.table)

# load data ---------------------------------------------------------------
activitiesFile <- "/Users/jri/Documents/ Research/Projects/simphony/ABCD/Sports and Activities NDA17 Release data.csv"

musicFields <- c("sai_p_music_school",	"sai_p_music_outside",	"sai_p_music_private",	"sai_p_music_self",	"sai_p_music_nyr",	"sai_p_music_nmonth",	"sai_p_music_perwk",	"sai_p_music_tspent",	"sai_p_music_p12")
subjFields <- c("id_redcap","site_name","asnt_timestamp","tlfb_age_calc_inmonths_l","demo_gender_id_v2_l","hr_score","sai_p_select_language___1")

data = read.csv(activitiesFile)
mdata <- data[,c(subjFields, musicFields)]

names(mdata) = sub("sai_p_music_","",names(mdata))
setnames(mdata,old=c("school","outside"),new=c("Group_school","Group_outside"))

mdata$anyMusic = mdata$Group_school | mdata$Group_outside | mdata$private | mdata$self
#mdata["anyMusic"][is.na(mdata["anyMusic"])] <- FALSE

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


# Descriptive analysis-------------------------------------------------------

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

#music % by region
mdata2 <- mdata %>% group_by(site_name) %>% mutate(percMusic = 100 * sum(anyMusic,na.rm=TRUE)/n(), musicN = sum(anyMusic,na.rm=TRUE), siteN = n())
print(arrange(aggregate(cbind(siteN,musicN, percMusic) ~ site_name, data=mdata2, FUN=mean), desc(percMusic)), row.names=FALSE)

#%music overall
mdata3 <- mdata %>% mutate(percMusic = 100 * sum(anyMusic,na.rm=TRUE)/n(), musicN = sum(anyMusic,na.rm=TRUE), siteN = n())
print(arrange(aggregate(cbind(siteN,musicN, percMusic) ~ site_name, data=mdata3, FUN=mean), desc(percMusic)), row.names=FALSE)

#mean practice amounts
onlymdata <- subset(mdata,anyMusic==TRUE)
summary(onlymdata[,c("nyr","nmonth","perwk","tspent")])

my.f = function(x) c(mean = mean(x,na.rm=TRUE), median = median(x,na.rm=TRUE))
onlymdata[, sapply(.SD, my.f), .SDcols = c("nyr","nmonth","perwk","tspent"), by = site_name]
