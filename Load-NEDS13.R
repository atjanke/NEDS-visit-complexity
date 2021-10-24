library(data.table)
library(tidytable)
library(tidyverse)

setwd("/Users/alexanderjanke/Data/neds/2013_NEDS")

neds13 <- fread("NEDS_2013_ED.csv")
colnames(neds13) <- c("DISCWT","HOSP_ED","KEY_ED","neds_stratum","CPT1","CPT2","CPT3","CPT4",
              "CPT5","CPT6","CPT7","CPT8","CPT9","CPT10","CPT11","CPT12","CPT13","CPT14","CPT15","CPTccs1","CPTccs2",
              "cptccs3","cptccs4","cptccs5","cptccs6","cptccs7","cptccs8","cptccs9","cptccs10","cptccs11","cptccs12",
              "cptccs13","cptccs14","cptccs15","ncpt","npr_ed","pclass_ed1","pclass_ed2","pclass_ed3","pclass_ed4",
              "pclass_ed5","pclass_ed6","pclass_ed7","pclass_ed8","pclass_ed9","pr_ed1","pr_ed2","pr_ed3","pr_ed4",
              "pr_ed5","pr_ed6","pr_ed7","pr_ed8","pr_ed9","PRCCSED1","PRCCSED2","PRCCSED3","PRCCSED4","PRCCSED5",
              "PRCCSED6","PRCCSED7","PRCCSED8","PRCCSED9")

neds13 <- neds13 %>% select.(KEY_ED,CPT1:CPT15,PRCCSED1:PRCCSED9)

core <- fread("NEDS_2013_Core.csv")

colnames(core) <- c("AGE","AMONTH","AWEEKEND","CHRON1","CHRON2","CHRON3","CHRON4","CHRON5","CHRON6",
                    "CHRON7","CHRON8","CHRON9","CHRON10","CHRON11","CHRON12","CHRON13","CHRON14","CHRON15",
                    "DIED_VIS","DISCWT","DISP_ED","DQTR","DX1","DX2","DX3","DX4","DX5","DX6","DX7","DX8",
                    "DX9","DX10","DX11","DX12","DX13","DX14","DX15","DXCCS1","DXCCS2","DXCCS3","DXCCS4",
                    "DXCCS5","DXCCS6","DXCCS7","DXCCS8","DXCCS9","DXCCS10","DXCCS11","DXCCS12","DXCCS13",
                    "DXCCS14","DXCCS15","ECODE1","ECODE2","ECODE3","ECODE4","EDEVENT","E_CCS1","E_CCS2",
                    "E_CCS3","E_CCS4","FEMALE","HCUPFILE","HOSP_ED",
                    "injury","injury_cut","injury_drown","injury_fall","injury_fire","injury_firearm","injury_machinery",
                    "injury_mvt","injury_nature","injury_poison","injury_severity","injury_struck","injury_suffocation",
                    "intent_assault","intent_self_harm","intent_unintentional","KEY_ED","multinjury","ndx","necode","NEDS_STR",
                    "PAY1","pay2","pl_nchs2006","TOTCHG_ED","YEAR","ZIPINC_QRTL")

core <- core %>% select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED,DXCCS1:DXCCS15)

neds13 <- neds13 %>%
  left_join.(core,by="KEY_ED") %>%
  select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED,DXCCS1:DXCCS15,CPT1:CPT15,PRCCSED1:PRCCSED9)
rm(core)

setwd("~/Box/EMF-Work/Project-Folder/NEDS-visit-complexity")

# Define a variable for the level of service code
source("Functions.R")
neds13 <- EM_Code(neds13)

saveRDS(neds13,"data-cleaned/neds13.rds")

subsample <- sample_n(neds13,1000000)
saveRDS(subsample,"data-cleaned/neds13-subsample.rds")


