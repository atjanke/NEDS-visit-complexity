library(data.table)
library(tidytable)
library(tidyverse)

setwd("/Users/alexanderjanke/Data/neds/2007_NEDS")

neds07 <- fread("NEDS_2007_ED.csv")

colnames(neds07) <- c("DISCWT","HCUPFILE","HOSP_ED","KEY_ED","CPT1","CPT2","CPT3","CPT4","CPT5",
                      "CPT6","CPT7","CPT8","CPT9","CPT10","CPT11","CPT12","CPT13","CPT14","CPT15",
                      "NCPT","NPR_ED","PCLS_ED1","PCLS_ED2","PCLS_ED3","PCLS_ED4","PCLS_ED5",
                      "PCLS_ED6","PCLS_ED7","PCLS_ED8","PCLS_ED9","PR_ED1","PR_ED2","PR_ED3",
                      "PR_ED4","PR_ED5"  ,"PR_ED6","PR_ED7","PR_ED8","PR_ED9","PRCCSED1","PRCCSED2",
                      "PRCCSED3","PRCCSED4","PRCCSED5","PRCCSED6","PRCCSED7","PRCCSED8","PRCCSED9")
neds07 <- neds07 %>% select.(KEY_ED,CPT1:CPT15,PRCCSED1:PRCCSED9)

core <- fread("NEDS_2007_Core.csv")

colnames(core) <- c("AGE","AMONTH","AWEEKEND","CHRON1","CHRON2","CHRON3","CHRON4","CHRON5","CHRON6",
                    "CHRON7","CHRON8","CHRON9","CHRON10","CHRON11","CHRON12","CHRON13","CHRON14","CHRON15",
                    "DIED_VIS","DISCWT","DISP_ED","DQTR","DX1","DX2","DX3","DX4","DX5","DX6","DX7","DX8",
                    "DX9","DX10","DX11","DX12","DX13","DX14","DX15","DXCCS1","DXCCS2","DXCCS3","DXCCS4",
                    "DXCCS5","DXCCS6","DXCCS7","DXCCS8","DXCCS9","DXCCS10","DXCCS11","DXCCS12","DXCCS13",
                    "DXCCS14","DXCCS15","ECODE1","ECODE2","ECODE3","ECODE4","EDEVENT","E_CCS1","E_CCS2",
                    "E_CCS3","E_CCS4","FEMALE","HCUPFILE","HOSP_ED","REGION","INTENT_S","KEY_ED","NDX",
                    "NECODE","NEDS_STR","PAY1","PAY2","PL_NCHS2","TOTCHGED","YEAR","ZIPINC_Q")

core <- core %>% select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED,DXCCS1:DXCCS15)

neds07 <- neds07 %>%
  left_join.(core,by="KEY_ED") %>%
  select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED,DXCCS1:DXCCS15,CPT1:CPT15,PRCCSED1:PRCCSED9)
rm(core)

setwd("~/Box/EMF-Work/Project-Folder/NEDS-visit-complexity")

# Define a variable for the level of service code
source("Functions.R")
neds07 <- EM_Code(neds07)

saveRDS(neds07,"data-cleaned/neds07.rds")

subsample <- sample_n(neds07,1000000)
saveRDS(subsample,"data-cleaned/neds07-subsample.rds")