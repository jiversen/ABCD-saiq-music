# make design matrices for music analysis
# 	baseline, only cases with music experience > 0 (omitting anyone with no music)
#	baseline, all cases (no music experience -> 0)
#	all data, same

library(tidyverse)
library(psych)
library(plyr)
library(dplyr)
library(data.table)
library(pracma)
#library(PerformanceAnalytics)

source('/home/jiversen/matlab/cmig-research-group/cmig_library/makeDEAPdemos.R')
source('/home/jiversen/matlab/cmig-research-group/cmig_library/makeDesign.R')

## SETUP ##

musicVars <- c("music_ageStarted","music_hrlifetime","arts_hrlifetime","sports_hrlifetime") #,"soc_hrlifetime")

#music, arts-music, sports
contvar<-c(musicVars[c(2, 3, 4)],'PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8','PC9','PC10')
fbase = 'ABCD_rel3.0_long_desmat_PCs_SES_artsport_music_hrlifetime'

contvar<-c(musicVars[c(2)],'PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8','PC9','PC10')
fbase = 'ABCD_rel3.0_long_desmat_PCs_SES_music_hrlifetime'

doOnlyBaseline = F
doSubsetMusicOnly = F
doQnorm = T

if (doOnlyBaseline) {
  time = c('baseline_year_1_arm_1')
  delta=NULL
  interact=NULL
} else {
  time = c('baseline_year_1_arm_1','2_year_follow_up_y_arm_1')
  delta=c('interview_age')
  interact=c('interview_age_base*interview_age_delta','interview_age_base*sex_at_birth','interview_age_delta*sex_at_birth')
  #interact=NULL
  fbase = paste0(fbase,'_T2')
  if (!is.null(interact)) { fbase = paste0(fbase,'i')}

}

###########

pcs_file = '/space/gwas-syn2/1/data/GWAS/ABCD/genotype/plink2.eigenvec'
datapath <- '/space/amdale/1/tmp/ABCD_cache/abcd-sync/3.0/tabulated/released/'
tab_imaging_path<-'/space/gwas-syn2/1/data/GWAS/ABCD/Images/NDA3.0_tabulated/'
music_path <- "/home/jiversen/Results/saiq/2021-02-27_16-01-48/nda3.0_activities.Rds" #'/home/jiversen/matlab/music/DEAP_Activity_Pheno.csv'


outpath = '/home/jiversen/matlab/music/designmat/'
outfile = paste0(outpath, fbase)

deapdemos<-makeDEAPdemos(datapath)

pcs = read.table(pcs_file, header=TRUE)
colnames(pcs)[colnames(pcs)=='IID'] = 'src_subject_id'

df = join(deapdemos, pcs, by='src_subject_id')

MRIinfo<-read.delim(paste0(datapath, 'abcd_mri01.txt'), skip=2, header=F, sep = "\t")
cols_MRIinfo<-read.delim(paste0(datapath, 'abcd_mri01.txt'), header=T, sep = "\t")
colnames(MRIinfo)<-colnames(cols_MRIinfo)
MRIinfo<-MRIinfo[,c('src_subject_id','eventname','mri_info_deviceserialnumber','mri_info_manufacturer','mri_info_softwareversion')]
MRIinfo[,'idevent']<-paste0(MRIinfo$src_subject_id, '_', MRIinfo$eventname)
MRIinfo<-MRIinfo[duplicated(MRIinfo$idevent)==FALSE,]
MRIinfo[which(MRIinfo$mri_info_deviceserialnumber==""),]<-NA
lvl<-unique(MRIinfo$mri_info_deviceserialnumber)
lvl<-lvl[is.na(lvl)==FALSE]
MRIinfo$mri_info_deviceserialnumber<-factor(MRIinfo$mri_info_deviceserialnumber, levels=lvl)
MRIinfo[which(MRIinfo$mri_info_softwareversion==""),]<-NA
lvl<-unique(MRIinfo$mri_info_softwareversion)
lvl<-lvl[is.na(lvl)==FALSE]
MRIinfo$mri_info_softwareversion<-factor(MRIinfo$mri_info_softwareversion, levels=lvl)
print('--MRI--')
df<-join(df, MRIinfo)


# Quantile normalization function
quant_norm = function(df_col){
  n = length(df_col)
  Fn = ecdf(df_col)
  return(qnorm(Fn(df_col)-0.5/n))
}

# convert NAs to 0 and qnorm (useful for sparse music covars with many missing values by non-endorsers, who obviously spend no time on it)
quant_norm_zeroNA = function(df_col){
  df_col[is.na(df_col)] <- 0
  return(quant_norm(df_col))
}

#TODO Factor out logic below to add arbitrary abcd vars
#add_vars <- function(fname, vars, qnorm=T) {
#}

# MUSIC
print('--Music--')
#music <- read.csv(music_path)
music <- readRDS(music_path)
#strip the "sports_activity_" prefix from variable names
names(music) = sub("sports_activity_","",names(music))
music <- music[,c('src_subject_id','eventname',musicVars)]
#correct arts to be arts besides music
mh <- music$music_hrlifetime;
mh[is.na(mh)]<-0
music$arts_hrlifetime <- max(0, music$arts_hrlifetime - mh)
df <- join(df, music, by=c('src_subject_id', 'eventname'))

# CBCL
fname <- 'abcd_cbcls01.txt'
cbclVars<-c('cbcl_scr_syn_external_r','cbcl_scr_syn_internal_r')
cbcl<-read.delim(paste0(datapath,fname), skip=2, header=F, sep = "\t")
cols_cbcl<-read.delim(paste0(datapath,fname), header=T, sep = "\t")
colnames(cbcl)<-colnames(cols_cbcl)
cbcl<-cbcl[,c('src_subject_id','eventname',cbclVars)]

df<-join(df, cbcl, by=c('src_subject_id','eventname'))

# COGNITION
fname <- 'abcd_tbss01.txt'
cogVars<-c('nihtbx_cryst_uncorrected','nihtbx_fluidcomp_uncorrected')
cog<-read.delim(paste0(datapath,fname), skip=2, header=F, sep = "\t")
cols_cog<-read.delim(paste0(datapath,fname), header=T, sep = "\t")
colnames(cog)<-colnames(cols_cog)
cog<-cog[,c('src_subject_id','eventname',cogVars)]

df<-join(df, cog, by=c('src_subject_id','eventname'))

################################

#colnames(df)[which(colnames(df)=='eventname')]<-'event_name'
#df$rel_family_id<-df$rel_family_id.x

df$sex_at_birth<-df$sex
#MUSIC ANALYSES
#allcovars<-c('interview_age',musicVars,cogVars,cbclVars,'PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8','PC9','PC10','sex_at_birth','mri_info_deviceserialnumber','mri_info_softwareversion', 'high.educ', 'household.income','hisp','mri_info_softwareversion','mri_info_manufacturer')
#sub_df<-na.omit(df[,c('src_subject_id','event_name','rel_family_id', allcovars)])

#handle subsetting (complete cases, etc)

#SUBSET only music_hrlifetime != NA
if (doSubsetMusicOnly) {
  hrs <- df$music_hrlifetime
  hasMusic = !is.na(hrs);
  sub_df<-df[hasMusic,]
  outfile = paste0(outfile,'_gt0')
} else {
  sub_df <- df
}

sub_df$age <- sub_df$interview_age

# QUANTILE NORMALIZATION (after subsetting)
if (doQnorm) {
  vars_qnorm = paste0(musicVars, '_qnorm')
  sub_df[, vars_qnorm] = as.numeric(apply(sub_df[, musicVars], 2, quant_norm_zeroNA))
  musicVars = vars_qnorm

  vars_qnorm = paste0(cbclVars, '_qnorm')
  sub_df[, vars_qnorm] = as.numeric(apply(sub_df[, cbclVars], 2, quant_norm))
  cbclVars = vars_qnorm

  vars_qnorm = paste0(cogVars, '_qnorm')
  sub_df[, vars_qnorm] = as.numeric(apply(sub_df[, cogVars], 2, quant_norm))
  cogVars = vars_qnorm

  outfile = paste0(outfile,'_qnorm')
}



#contvar<-c(musicVars,cogVars,cbclVars,'PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8','PC9','PC10')

if (doOnlyBaseline) {
  #contvar <- c('interview_age', contvar) #only added in case of baseline only, since it is in the delta var otherwise
  outfile = paste0(outfile,'_noA')
}
catvar<-c('sex_at_birth','mri_info_deviceserialnumber','mri_info_softwareversion', 'high.educ', 'household.income','hisp')

outfile = paste0(outfile,'.txt')

print(sprintf('Make design: %s',outfile))
outmat<-makeDesign(sub_df, outfile, time, contvar=contvar, catvar=catvar,  delta=delta, interact=interact, subjs=NULL, demean=FALSE, complete=TRUE) #softid=TRUE, rm_na=TRUE,

write.table(outmat, outfile, col.names=TRUE, row.names=FALSE, sep='\t')


