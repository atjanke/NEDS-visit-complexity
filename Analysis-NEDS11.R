library(dplyr)
library(data.table)
library(tidytable)
library(tidyverse)
library(tictoc)

# Load full NEDS RDS file
tic()
neds11 <- readRDS("__data-cleaned/neds11.rds")
toc()

# Remove age<18
neds11 <- neds11 %>% 
  mutate.(AGE = case_when(
    AGE<0 ~ NA_integer_,
    T ~ AGE
  )) 
neds11 <- neds11 %>%
  filter.(AGE>=18)

# Make a list of HOSP_ED with consistent E/M coding
list_of_sites <- neds11 %>%
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
neds11 <- neds11 %>% filter.(HOSP_ED %in% list_of_sites$HOSP_ED)






tic()
core <- fread("../../../../Data/neds/2011_NEDS/NEDS_2011_Core.csv")
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

core <- core %>% select.(KEY_ED,DX1:DX15)
toc()

neds11 <- neds11 %>%
  left_join.(core,by="KEY_ED")
rm(core)
gc()



# Tack on diagnosis codes and generate Elixhauser Comorbidity Index
# https://github.com/ellessenne/comorbidity

tic()
diag_list <- neds11 %>%
  #  slice(1:1100000) %>%
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
  select.(KEY_ED,score,chf:depre) %>%
  rename.(ELIX = score) %>%
  mutate.(ELIX = case_when(
    ELIX<0 ~ 0,
    T ~ ELIX))
rm(diag_list)
gc()
toc()

neds11 <- neds11 %>%
  left_join.(elix,by="KEY_ED")
rm(elix)
gc()

# Filter out DISP_ED --> short-term hospital
neds11 <- neds11 %>% filter.(DISP_ED!=2)

# Save the analysis sample
saveRDS(neds11,"__data-analysis/neds11-analysis.rds")

# Save a stratified subsample
library(splitstackshape)
neds11_strat <- stratified(neds11, c("HOSP_ED"), 0.1)
saveRDS(neds11_strat,"__data-analysis/neds11-analysis-subsample.rds")

# Save the hospital-level data
data <- fread("../../../../Data/neds/2011_NEDS/NEDS_2011_Hospital.csv")
colnames(data) <-  c("DISCWT","HOSPWT","CONTROL","HOSP_ED","REGION","TRAUMA",
                     "URCAT4","UR_TEACH","NEDS_STR","N_DISC_U","N_HOSP_U",
                     "S_DISC_U","S_HOSP_U","EDVISITS","YEAR")

hosp11 <- list_of_sites %>% left_join.(data,by="HOSP_ED")

saveRDS(hosp11,"__data-analysis/hosp11-analysis.rds")