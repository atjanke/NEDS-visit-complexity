library(data.table)
library(tidytable)
library(tidyverse)
library(splitstackshape)

# Quarters 1-3
setwd("/Users/alexanderjanke/Data/neds/2016_NEDS")

neds16 <- fread("NEDS_2016_ED.csv")
colnames(neds16) <- c("hosp_ed","KEY_ED","CPT1","CPT2","CPT3","CPT4","CPT5","CPT6","CPT7",
                      "CPT8","CPT9","CPT10","CPT11","CPT12","CPT13","CPT14","CPT15","PRCCSED1",
                      "PRCCSED2","PRCCSED3","PRCCSED4","PRCCSED5","PRCCSED6","PRCCSED7","PRCCSED8",
                      "PRCCSED9","PRCCSED10","PRCCSED11","PRCCSED12","PRCCSED13","PRCCSED14","PRCCSED15","nCPT")

neds16 <- neds16 %>% select.(KEY_ED,CPT1:CPT15,PRCCSED1:PRCCSED9)


core <- fread("NEDS_2016_Core.csv")

colnames(core) <- c("AGE","amonth","aweekend","died_visit","DISCWT","DISP_ED","dqtr","dxver",
                    "edevent","FEMALE","hcupfile","HOSP_ED","DX1","DX2","DX3","DX4","DX5","DX6",
                    "DX7","DX8","DX9","DX10","DX11","DX12","DX13","DX14","DX15","i10_dx16","i10_dx17",
                    "i10_dx18","i10_dx19","i10_dx20","i10_dx21","i10_dx22","i10_dx23","i10_dx24",
                    "i10_dx25","i10_dx26","i10_dx27","i10_dx28","i10_dx29","i10_dx30","i10_ecause1",
                    "i10_ecause2","i10_ecause3","i10_ecause4","i10_ndx","i10_necause","KEY_ED","NEDS_STR",
                    "PAY1","pay2","pl_nchs","TOTCHG_ED","year","ZIPINC_QRTL")

core <- core %>% select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED,DX1:DX15)


neds16 <- neds16 %>%
  left_join.(core,by="KEY_ED") %>%
  select.(HOSP_ED,KEY_ED,DISCWT,NEDS_STR,AGE,FEMALE,PAY1,DISP_ED,DX1:DX15,CPT1:CPT15,PRCCSED1:PRCCSED9)


setwd("~/Box/EMF-Work/Project-Folder/NEDS-visit-complexity")

# Build CCSR Codes
setwd("~/Box/EMF-Work/Project-Folder/NEDS-visit-complexity")
ccsr <- fread("ccsr.csv",select=c(1:3))
colnames(ccsr) <- c("DX","DESCRIPTION","CCSR") 
ccsr <- ccsr %>%
  select.(DX,CCSR)
ccsr[,1] <- lapply(ccsr[,1],gsub,pattern="\\'",replacement='')
ccsr[,2] <- lapply(ccsr[,2],gsub,pattern="\\'",replacement='')

# Build 
source("Functions.R")
neds16 <- EM_Code(neds16)


saveRDS(neds16,"__data-cleaned/neds16.rds")

subsample <- stratified(neds16,c("HOSP_ED"),0.1)
saveRDS(subsample,"__data-cleaned/neds16-subsample.rds")
  
    