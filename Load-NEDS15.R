library(data.table)
library(tidytable)
library(tidyverse)
library(splitstackshape)

# Quarters 1-3
setwd("/Users/alexanderjanke/Data/neds/2015_NEDS")

neds15_Q1Q3 <- fread("NEDS_2015Q1Q3_ED.csv")

colnames(neds15_Q1Q3) <- c(
              "chron1","chron2","chron3","chron4","chron5","chron6","chron7","chron8","chron9","chron10","chron11","chron12",
              "chron13","chron14","chron15","chron16","chron17","chron18","chron19","chron20","chron21","chron22","chron23",
              "chron24","chron25","chron26","chron27","chron28","chron29","chron30","CPT1","CPT2","CPT3","CPT4","CPT5","CPT6",
              "CPT7","CPT8","CPT9","CPT10","CPT11","CPT12","CPT13","CPT14","CPT15","PRCCSED1","PRCCSED2","PRCCSED3","PRCCSED4","PRCCSED5",
              "PRCCSED6","PRCCSED7","PRCCSED8","PRCCSED9","PRCCSED10","PRCCSED11","PRCCSED12","PRCCSED13","PRCCSED14","PRCCSED15","DX1","DX2",
              "DX3","DX4","DX5","DX6","DX7","DX8","DX9","DX10","DX11","DX12","DX13","DX14","DX15","DX16","DX17","DX18","DX19","DX20",
              "DX21","DX22","DX23","DX24","DX25","DX26","DX27","DX28","DX29","DX30","DXCCS1","DXCCS2","DXCCS3","DXCCS4","DXCCS5",
              "DXCCS6","DXCCS7","DXCCS8","DXCCS9","DXCCS10","DXCCS11","DXCCS12","DXCCS13","DXCCS14","DXCCS15","DXCCS16","DXCCS17",
              "DXCCS18","DXCCS19","DXCCS20","DXCCS21","DXCCS22","DXCCS23","DXCCS24","DXCCS25","DXCCS26","DXCCS27","DXCCS28","DXCCS29",
              "DXCCS30","dxver","ecode1","ecode2","ecode3","ecode4","e_ccs1","e_ccs2","e_ccs3","e_ccs4","hcupfile","hosp_ed","injury",
              "injury_cut","injury_drown","injury_fall","injury_fire","injury_firearm","injury_machinery","injury_mvt","injury_nature",
              "injury_poison","injury_severity","injury_struck","injury_suffocation","intent_assault","intent_self_harm","intent_unintentional",
              "KEY_ED","multinjury","ncpt","ndx","necode")

neds15_Q1Q3 <- neds15_Q1Q3 %>% select.(KEY_ED,CPT1:CPT15,DXCCS1:DXCCS15,PRCCSED1:PRCCSED9)

core <- fread("NEDS_2015_Core.csv")

colnames(core) <- c("AGE","amonth","aweekend","died_visit","DISCWT","DISP_ED","dqtr","edevent","FEMALE","hcupfile",
                    "HOSP_ED","KEY_ED","NEDS_STR","PAY1","PAY2","pl_nchs","TOTCHG_ED","YEAR","ZIPINC_QRTL")

core <- core %>% select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED)

neds15_Q1Q3 <- neds15_Q1Q3 %>%
  left_join.(core,by="KEY_ED") %>%
  select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED,DXCCS1:DXCCS15,CPT1:CPT15,PRCCSED1:PRCCSED9)



# Quarter 4
setwd("/Users/alexanderjanke/Data/neds/2015_NEDS")

neds15_Q4 <- fread("NEDS_2015Q4_ED.csv")

colnames(neds15_Q4) <- c("CPT1","CPT2","CPT3","CPT4","CPT5","CPT6",
  "CPT7","CPT8","CPT9","CPT10","CPT11","CPT12","CPT13","CPT14","CPT15",
  "PRCCSED1","PRCCSED2","PRCCSED3","PRCCSED4","PRCCSED5",
  "PRCCSED6","PRCCSED7","PRCCSED8","PRCCSED9","PRCCSED10","PRCCSED11","PRCCSED12",
  "PRCCSED13","PRCCSED14","PRCCSED15","dxver","hcupfile","hosp_ed","DX1","DX2",
  "DX3","DX4","DX5","DX6","DX7","DX8","DX9","DX10","DX11","DX12","DX13","DX14",
  "DX15","i10_dx16","i10_dx17","i10_dx18","i10_dx19","i10_dx20","i10_dx21","i10_dx22",
  "i10_dx23","i10_dx24","i10_dx25","i10_dx26","i10_dx27","i10_dx28","i10_dx29","i10_dx30",
  "i10_ecause1","i10_ecause2","i10_ecause3","i10_ecause4","i10_ndx","i10_necause","KEY_ED","NCPT")


neds15_Q4 <- neds15_Q4 %>%
  left_join.(core,by="KEY_ED") %>%
  select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED,DX1:DX15,CPT1:CPT15,PRCCSED1:PRCCSED9)


# Build CCSR Codes
setwd("~/Box/EMF-Work/Project-Folder/NEDS-visit-complexity")
ccsr <- fread("ccsr.csv",select=c(1:3))
colnames(ccsr) <- c("DX","DESCRIPTION","CCSR") 
ccsr <- ccsr %>%
  select.(DX,CCSR)
ccsr[,1] <- lapply(ccsr[,1],gsub,pattern="\\'",replacement='')
ccsr[,2] <- lapply(ccsr[,2],gsub,pattern="\\'",replacement='')

neds15_Q4 <- neds15_Q4 %>%
  rename.(DX=DX1) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS1=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX2) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS2=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX3) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS3=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX4) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS4=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX5) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS5=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX6) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS6=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX7) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS7=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX8) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS8=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX9) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS9=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX10) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS10=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX11) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS11=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX12) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS12=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX13) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS13=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX14) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS14=CCSR) %>% select.(-DX) %>%
  rename.(DX=DX15) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS15=CCSR) %>% select.(-DX)

neds15 <- rbindlist(list(neds15_Q1Q3,neds15_Q4),use.names=TRUE)

# Define a variable for the level of service code
source("Functions.R")
neds15 <- EM_Code(neds15)


saveRDS(neds15,"__data-cleaned/neds15.rds")

subsample <- stratified(neds15,c("HOSP_ED"),0.1)
saveRDS(subsample,"__data-cleaned/neds15-subsample.rds")



