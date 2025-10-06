rm(list=ls())
library(meta)
library(dplyr)

# Overall Survival , OS
DATA_OS=readxl::read_xlsx("F:/Meta_Analysis_FINAL/os_pfs_orr_meta_analysis_ovarian_cancer.xlsx",sheet = "OS")#Path to the excel file
data_os <- DATA_OS
data_os
View(data_os)

help(metagen)

# Only Fixed Effect model # random = FALSE , default: fixed or common = TRUE
fe_os<-metagen(
  median_os, se_os, data = data_os,sm="Median OS",
  #method = "Inverse",
  #common =gs("common"),
  random= FALSE,
  studlab = data_os$Study,subgroup=data_os$Comparator,
  method.tau = "REML", ## method to calculate Tau
  #method.common.ci = "classic", ## method to calculate estimator's CI
)
summary(fe_os)

# Only Random Effect model # common = FALSE, random = TRUE

re_os<-metagen(
  median_os, se_os, data = data_os,sm="Median OS",
  #method = "Inverse",
  #common =gs("common"),
  common=FALSE,
  random= TRUE,
  studlab = data_os$Study,subgroup=data_os$Comparator,
  method.tau = "REML", ## method to calculate Tau
  #method.common.ci = "classic", ## method to calculate estimator's CI
)
summary(re_os)


# for fixed
forest(
  fe_os,
  fontsize = 7,
  spacing = 0.6,
  ref_line = 1,
  layout = "RevMan5",
  col.square = "lightblue",
  col.square.lines = "black",
  header.line = "",
  ff.studyc = "plain",
  colgap.right = 10,
  fs.heading = 8,
  fs.random = 8,
  fs.hetstat = 8,
  test.overall.common=TRUE,
  test.overall.random=TRUE, addrows = 3
  #leftcols = c("studlab", "StudyName","TE", "seTE","w.common","effect.ci"),
  #leftlabs = c("Author","Trial", "median OS", "SE","Weight","mOS [95% CI]")
  
)

# for random
forest(
  re_os,
  fontsize = 7,
  spacing = 0.6,
  ref_line = 1,
  layout = "RevMan5",
  col.square = "lightblue",
  col.square.lines = "black",
  header.line = "",
  ff.studyc = "plain",
  colgap.right = 10,
  fs.heading = 8,
  fs.random = 8,
  fs.hetstat = 8,
  test.overall.common=TRUE,
  test.overall.random=TRUE, addrows = 3
  #leftcols = c("studlab", "StudyName","TE", "seTE","w.common","effect.ci"),
  #leftlabs = c("Author","Trial", "median OS", "SE","Weight","mOS [95% CI]")
  
)

# Progression Free Survival , PFS

DATA_PFS=readxl::read_xlsx("F:/Meta_Analysis_FINAL/os_pfs_orr_meta_analysis_ovarian_cancer.xlsx",sheet = "PFS")#Path to the excel file
data_pfs <- DATA_PFS
data_pfs
View(data_pfs)

# Only Fixed Effect model # random = FALSE , default: fixed or common = TRUE
fe_pfs<-metagen(
  median_pfs, se_pfs, data = data_pfs,sm="Median PFS",
  #method = "Inverse",
  #common =gs("common"),
  random= FALSE,
  studlab = data_pfs$Study,subgroup=data_pfs$Comparator,
  method.tau = "REML", ## method to calculate Tau
  #method.common.ci = "classic", ## method to calculate estimator's CI
)
summary(fe_pfs)

# Only Random Effect model # common = FALSE, random = TRUE

re_pfs<-metagen(
  median_pfs, se_pfs, data = data_pfs,sm="Median PFS",
  #method = "Inverse",
  #common =gs("common"),
  common=FALSE,
  random= TRUE,
  studlab = data_pfs$Study,subgroup=data_pfs$Comparator,
  method.tau = "REML", ## method to calculate Tau
  #method.common.ci = "classic", ## method to calculate estimator's CI
)
summary(re_pfs)

# for fixed
forest(
  fe_pfs,
  fontsize = 7,
  spacing = 0.6,
  ref_line = 1,
  layout = "RevMan5",
  col.square = "lightblue",
  col.square.lines = "black",
  header.line = "",
  ff.studyc = "plain",
  colgap.right = 10,
  fs.heading = 8,
  fs.random = 8,
  fs.hetstat = 8,
  test.overall.common=TRUE,
  test.overall.random=TRUE, addrows = 3
  #leftcols = c("studlab", "StudyName","TE", "seTE","w.common","effect.ci"),
  #leftlabs = c("Author","Trial", "median OS", "SE","Weight","mOS [95% CI]")
  
)

# for random
forest(
  re_pfs,
  fontsize = 7,
  spacing = 0.6,
  ref_line = 1,
  layout = "RevMan5",
  col.square = "lightblue",
  col.square.lines = "black",
  header.line = "",
  ff.studyc = "plain",
  colgap.right = 10,
  fs.heading = 8,
  fs.random = 8,
  fs.hetstat = 8,
  test.overall.common=TRUE,
  test.overall.random=TRUE, addrows = 3
  #leftcols = c("studlab", "StudyName","TE", "seTE","w.common","effect.ci"),
  #leftlabs = c("Author","Trial", "median OS", "SE","Weight","mOS [95% CI]")
  
)

# ORR , only fixed effect model

DATA_ORR=readxl::read_xlsx("F:/Meta_Analysis_FINAL/os_pfs_orr_meta_analysis_ovarian_cancer.xlsx",sheet = "ORR")#Path to the excel file
library(dplyr)
data_orr<-DATA_ORR
View(data_orr)
orr_fe<-metaprop(
  event, samplesize, data = data_orr,
  method = "Inverse",
  level = gs("level"),
  random = FALSE,
  studlab = data_orr$Study,
  subgroup=data_orr$Comparator,
  method.tau = "ML", ## method to calculate Tau
  method.random.ci = "classic", ## method to calculate estimator's CI
)

summary(orr_fe)

forest(
  orr_fe,sortvar=event,
  fontsize = 7,
  spacing = 0.6,
  ref_line = 1,
  layout = "RevMan5",
  col.square = "lightblue",
  col.square.lines = "black",
  header.line = "",
  ff.studyc = "plain",
  colgap.right = 10,
  fs.heading = 8,
  fs.random = 8,
  fs.hetstat = 8,
  test.overall.common=TRUE,
  test.overall.random=FALSE, addrows = 3,
  # leftcols = c("studlab", "StudyName","event", "n","w.common","effect.ci"),
  # leftlabs = c("Author","Trial", "Events", "Total","Weight","ORR [95% CI]")
  
)

# ORR , only random effect model , # common = FALSE, random = TRUE

orr_re<-metaprop(
  event, samplesize, data = data_orr,
  method = "Inverse",
  level = gs("level"),
  common = FALSE,
  random = TRUE,
  studlab = data_orr$Study,
  subgroup=data_orr$Comparator,
  method.tau = "ML", ## method to calculate Tau
  method.random.ci = "classic", ## method to calculate estimator's CI
)

summary(orr_re)

forest(
  orr_re,sortvar=event,
  fontsize = 7,
  spacing = 0.6,
  ref_line = 1,
  layout = "RevMan5",
  col.square = "lightblue",
  col.square.lines = "black",
  header.line = "",
  ff.studyc = "plain",
  colgap.right = 10,
  fs.heading = 8,
  fs.random = 8,
  fs.hetstat = 8,
  test.overall.common=TRUE,
  test.overall.random=FALSE, addrows = 3,
  # leftcols = c("studlab", "StudyName","event", "n","w.common","effect.ci"),
  # leftlabs = c("Author","Trial", "Events", "Total","Weight","ORR [95% CI]")
  
)







