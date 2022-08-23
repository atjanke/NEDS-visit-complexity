library(dplyr)
library(data.table)
library(tidytable)
library(tidyverse)
library(tictoc)

# Load full NEDS RDS file
tic()
neds15 <- readRDS("__data-cleaned/neds15.rds")
toc()

# Remove age<18
neds15 <- neds15 %>% 
  mutate.(AGE = case_when(
    AGE<0 ~ NA_integer_,
    T ~ AGE
  )) 
neds15 <- neds15 %>%
  filter.(AGE>=18)

# Make a list of HOSP_ED with consistent E/M coding
list_of_sites <- neds15 %>%
  mutate.(EM_Reported = case_when(
    EM_Code=="Not listed" ~ 0,
    T~1
  )) %>%
  summarise.(
    reporting = sum(EM_Reported)/n.(),.by="HOSP_ED")

list_of_sites <- list_of_sites %>%
  filter.(reporting>0.95) %>%
  select.(HOSP_ED)

# Filter to only include sites with consistent reporting
neds15 <- neds15 %>% filter.(HOSP_ED %in% list_of_sites$HOSP_ED)

# Identify rows for ICD-9 and ICD-10
library(stringr)
neds15 <- neds15 %>%
  mutate.(ICD = case_when(
    str_detect(DXCCS1,"^[:digit:]+$") ~ "9", T~"10"))

neds15_Q1Q3 <- neds15[ICD=="9" ]
neds15_Q4   <- neds15[ICD=="10"]
rm(neds15)
gc()

setwd("~/Box/EMF-Work/Project-Folder/NEDS-visit-complexity")
source("01_Load-Clean-Data/Analysis-NEDS15_Q1Q3.R")
setwd("~/Box/EMF-Work/Project-Folder/NEDS-visit-complexity")
source("01_Load-Clean-Data/Analysis-NEDS15_Q4.R")

neds15 <-rbindlist(list(neds15_Q1Q3,neds15_Q4))
rm(neds15_Q1Q3,neds15_Q4)
gc()


setwd("~/Box/EMF-Work/Project-Folder/NEDS-visit-complexity")

# Filter out DISP_ED --> short-term hospital
neds15 <- neds15 %>% filter.(DISP_ED!=2)

# Save the analysis sample
saveRDS(neds15,"__data-analysis/neds15-analysis.rds")

# Save a stratified subsample
library(splitstackshape)
neds15_strat <- stratified(neds15, c("HOSP_ED"), 0.1)
saveRDS(neds15_strat,"__data-analysis/neds15-analysis-subsample.rds")

# Save the hospital-level data
data <- fread("../../../../Data/neds/2015_NEDS/NEDS_2015_Hospital.csv")
colnames(data) <- c("DISCWT","HOSPWT","CONTROL","HOSP_ED","REGION","TRAUMA",
                    "URCAT4","UR_TEACH","NEDS_STR","N_DISC_U","N_HOSP_U",
                    "S_DISC_U","S_HOSP_U","EDVISITS","YEAR")

hosp15 <- list_of_sites %>% left_join.(data,by="HOSP_ED")


saveRDS(hosp15,"__data-analysis/hosp15-analysis.rds")