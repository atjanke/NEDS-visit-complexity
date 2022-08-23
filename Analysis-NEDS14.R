library(dplyr)
library(data.table)
library(tidyverse)
library(tictoc)

# Load full NEDS RDS file
tic()
neds14 <- readRDS("__data-cleaned/neds14.rds")
toc()

# Remove age<18
neds14 <- neds14 %>% 
  mutate.(AGE = case_when(
    AGE<0 ~ NA_integer_,
    T ~ AGE
  )) 
neds14 <- neds14 %>%
  filter.(AGE>=18)

# Make a list of HOSP_ED with consistent E/M coding
list_of_sites <- neds14 %>%
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
neds14 <- neds14 %>% filter.(HOSP_ED %in% list_of_sites$HOSP_ED)






tic()
core <- fread("../../../../Data/neds/2014_NEDS/NEDS_2014_Core.csv")
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

core <- core %>% select.(KEY_ED,DX1:DX15)
toc()

neds14 <- neds14 %>%
  left_join.(core,by="KEY_ED")
rm(core)
gc()




# Tack on diagnosis codes and generate Elixhauser Comorbidity Index
# https://github.com/ellessenne/comorbidity

tic()
diag_list <- neds14 %>%
  #  slice(1:1400000) %>%
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

neds14 <- neds14 %>%
  left_join.(elix,by="KEY_ED")
rm(elix)
gc()

# Filter out DISP_ED --> short-term hospital
neds14 <- neds14 %>% filter.(DISP_ED!=2)

# Save the analysis sample
saveRDS(neds14,"__data-analysis/neds14-analysis.rds")

# Save a stratified subsample
library(splitstackshape)
neds14_strat <- stratified(neds14, c("HOSP_ED"), 0.1)
saveRDS(neds14_strat,"__data-analysis/neds14-analysis-subsample.rds")

# Save the hospital-level data
data <- fread("../../../../Data/neds/2014_NEDS/NEDS_2014_Hospital.csv")
colnames(data) <- c("DISCWT","HOSPWT","CONTROL","HOSP_ED","REGION","TRAUMA",
                    "URCAT4","UR_TEACH","NEDS_STR","N_DISC_U","N_HOSP_U",
                    "S_DISC_U","S_HOSP_U","EDVISITS","YEAR")

hosp14 <- list_of_sites %>% left_join.(data,by="HOSP_ED")


saveRDS(hosp14,"__data-analysis/hosp14-analysis.rds")