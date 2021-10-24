library(dplyr)
library(data.table)
library(tidyverse)
library(tictoc)

# Load full NEDS RDS file
tic()
neds08 <- readRDS("data-cleaned/neds08.rds")
toc()

# Remove age<18
neds08 <- neds08 %>% 
  mutate.(AGE = case_when(
    AGE<0 ~ NA_integer_,
    T ~ AGE
  )) 
neds08 <- neds08 %>%
  filter.(AGE>=18)

# Make a list of HOSP_ED with consistent E/M coding
list_of_sites <- neds08 %>%
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
neds08 <- neds08 %>% filter.(HOSP_ED %in% list_of_sites$HOSP_ED)






tic()
core <- fread("../../../../Data/neds/2008_NEDS/NEDS_2008_Core.csv")
colnames(core) <- c("AGE","AMONTH","AWEEKEND","CHRON1","CHRON2","CHRON3","CHRON4","CHRON5","CHRON6",
                    "CHRON7","CHRON8","CHRON9","CHRON10","CHRON11","CHRON12","CHRON13","CHRON14","CHRON15",
                    "DIED_VIS","DISCWT","DISP_ED","DQTR","DX1","DX2","DX3","DX4","DX5","DX6","DX7","DX8",
                    "DX9","DX10","DX11","DX12","DX13","DX14","DX15","DXCCS1","DXCCS2","DXCCS3","DXCCS4",
                    "DXCCS5","DXCCS6","DXCCS7","DXCCS8","DXCCS9","DXCCS10","DXCCS11","DXCCS12","DXCCS13",
                    "DXCCS14","DXCCS15","ECODE1","ECODE2","ECODE3","ECODE4","EDEVENT","E_CCS1","E_CCS2",
                    "E_CCS3","E_CCS4","FEMALE","HCUPFILE","HOSP_ED","REGION","INTENT_S","KEY_ED","NDX",
                    "NECODE","NEDS_STR","PAY1","PAY2","PL_NCHS2","TOTCHGED","YEAR","ZIPINC_Q")
core <- core %>% select.(KEY_ED,DX1:DX15)
toc()

neds08 <- neds08 %>%
  left_join.(core,by="KEY_ED")
rm(core)
gc()



# Tack on diagnosis codes and generate Elixhauser Comorbidity Index
# https://github.com/ellessenne/comorbidity

tic()
diag_list <- neds08 %>%
  #  slice(1:0800000) %>%
  select.(KEY_ED, DX1:DX15) %>%
  pivot_longer.(DX1:DX15) %>%
  rename(code=value) %>%
  select.(-name) %>%
  data.frame()
toc()

tic()
# https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf
library(comorbidity)
elix <- comorbidity(x=diag_list,id="KEY_ED",code="code",score="elixhauser",icd="icd9",assign0=FALSE) %>%
  select.(KEY_ED,score) %>%
  rename.(ELIX = score) %>%
  mutate.(ELIX = case_when(
    ELIX<0 ~ 0,
    T ~ ELIX))
rm(diag_list)
gc()
toc()

neds08 <- neds08 %>%
  left_join.(elix,by="KEY_ED")
rm(elix)
gc()

# Save the analysis sample
saveRDS(neds08,"data-analysis/neds08-analysis.rds")

# Save a stratified subsample
library(splitstackshape)
neds08_strat <- stratified(neds08, c("HOSP_ED"), 0.1)
saveRDS(neds08_strat,"data-analysis/neds08-analysis-subsample.rds")

# Save the hospital-level data
data <- fread("../../../../Data/neds/2008_NEDS/NEDS_2008_Hospital.csv")
colnames(data) <- c("DISCWT","HOSPWT","CONTROL","HOSP_ED","REGION","TRAUMA",
                    "URCAT4","UR_TEACH","NEDS_STR","N_DISC_U","N_HOSP_U",
                    "S_DISC_U","S_HOSP_U","EDVISITS","YEAR")

hosp08 <- list_of_sites %>% left_join.(data,by="HOSP_ED")

saveRDS(hosp08,"data-analysis/hosp08-analysis.rds")