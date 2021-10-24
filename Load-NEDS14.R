library(data.table)
library(tidytable)
library(tidyverse)

setwd("/Users/alexanderjanke/Data/neds/2014_NEDS")

neds14 <- fread("NEDS_2014_ED.csv")
colnames(neds14) <- c("DISC_WT","hosp_ed","KEY_ED","NEDS_STR","CPT1","CPT2","CPT3","CPT4","CPT5","CPT6","CPT7","CPT8","CPT9",
                      "CPT10","CPT11","CPT12","CPT13","CPT14","CPT15","cptccs1","cptccs2","cptccs3","cptccs4",
                      "cptccs5","cptccs6","cptccs7","cptccs8","cptccs9","cptccs10","cptccs11","cptccs12","cptccs13",
                      "cptccs14","cptccs15","ncpt","npr_ed","pclass_ed1","pclass_ed2","pclass_ed3","pclass_ed4",
                      "pclass_ed5","pclass_ed6","pclass_ed7","pclass_ed8","pclass_ed9","pr_ed1","pr_ed2","pr_ed3",
                      "pr_ed4","pr_ed5","pr_ed6","pr_ed7","pr_ed8","pr_ed9","PRCCSED1","PRCCSED2","PRCCSED3","PRCCSED4",
                      "PRCCSED5","PRCCSED6","PRCCSED7","PRCCSED8","PRCCSED9")

neds14 <- neds14 %>% select.(KEY_ED,CPT1:CPT15,PRCCSED1:PRCCSED9)

core <- fread("NEDS_2014_Core.csv")

colnames(core) <- c('AGE','AMONTH','AWEEKEND','CHRON1','CHRON2','CHRON3','CHRON4','CHRON5','CHRON6','CHRON7','CHRON8',
                    'CHRON9','CHRON10','CHRON11','CHRON12','CHRON13','CHRON14','CHRON15','CHRON16','CHRON17','CHRON18',
                    'CHRON19','CHRON20','CHRON21','CHRON22','CHRON23','CHRON24','CHRON25','CHRON26','CHRON27','CHRON28',
                    'CHRON29','CHRON30','DIED_VISIT','DISCWT','DISP_ED','DQTR','DX1','DX2','DX3','DX4','DX5','DX6','DX7',
                    'DX8','DX9','DX10','DX11','DX12','DX13','DX14','DX15','DX16','DX17','DX18','DX19','DX20','DX21','DX22',
                    'DX23','DX24','DX25','DX26','DX27','DX28','DX29','DX30','DXCCS1','DXCCS2','DXCCS3','DXCCS4','DXCCS5',
                    'DXCCS6','DXCCS7','DXCCS8','DXCCS9','DXCCS10','DXCCS11','DXCCS12','DXCCS13','DXCCS14','DXCCS15','DXCCS16',
                    'DXCCS17','DXCCS18','DXCCS19','DXCCS20','DXCCS21','DXCCS22','DXCCS23','DXCCS24','DXCCS25','DXCCS26','DXCCS27',
                    'DXCCS28','DXCCS29','DXCCS30','ECODE1','ECODE2','ECODE3','ECODE4','EDEVENT','E_CCS1','E_CCS2','E_CCS3',
                    'E_CCS4','FEMALE','HCUPFILE','HOSP_ED','INJUR','INJURY_CUT','INJURY_DROWN','INJURY_FALL','INJURY_FIRE',
                    'INJURY_FIREARM','INJURY_MACHINERY','INJURY_MVT','INJURY_NATURE','INJURY_POISON','INJURY_SEVERITY',
                    'INJURY_STRUCK','INJURY_SUFFOCATIO','INTENT_ASSAULT','INTENT_SELF_HARM','INTENT_UNINTENTIONA','KEY_ED',
                    'MULTINJURY','NDX','NECODE','NEDS_STR','PAY1','PAY2','PL_NCHS','TOTCHG_ED','YEAR','ZIPINC_QRTL')


core <- core %>% select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED,DXCCS1:DXCCS15)

neds14 <- neds14 %>%
  left_join.(core,by="KEY_ED") %>%
  select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED,DXCCS1:DXCCS15,CPT1:CPT15,PRCCSED1:PRCCSED9)
rm(core)

setwd("~/Box/EMF-Work/Project-Folder/NEDS-visit-complexity")

# Define a variable for the level of service code
source("Functions.R")
neds14 <- EM_Code(neds14)

saveRDS(neds14,"data-cleaned/neds14.rds")

subsample <- sample_n(neds14,1000000)
saveRDS(subsample,"data-cleaned/neds14-subsample.rds")