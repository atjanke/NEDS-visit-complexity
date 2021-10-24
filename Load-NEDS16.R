library(data.table)
library(tidytable)
library(tidyverse)

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

library(tictoc)
tic()
neds16 <- neds16 %>%
  rename.(DX=DX1) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS1=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX2) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS2=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX3) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS3=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX4) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS4=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX5) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS5=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX6) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS6=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX7) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS7=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX8) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS8=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX9) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS9=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX10) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS10=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX11) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS11=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX12) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS12=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX13) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS13=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX14) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS14=CCSR) %>% select.(-DX)
gc()
toc()

tic()
neds16 <- neds16 %>%
  rename.(DX=DX15) %>% left_join.(ccsr,by=c("DX")) %>% rename.(DXCCS15=CCSR) %>% select.(-DX)
gc()
toc()



# Build 
source("Functions.R")
neds16 <- EM_Code(neds16)


saveRDS(neds16,"data-cleaned/neds16.rds")

subsample <- sample_n(neds16,1000000)
saveRDS(subsample,"data-cleaned/neds16-subsample.rds")
  
    