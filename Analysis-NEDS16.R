library(dplyr)
library(data.table)
library(tidytable)
library(tidyverse)
library(tictoc)

# Load full NEDS RDS file
tic()
neds16 <- readRDS("data-cleaned/neds16.rds")
toc()

# Remove age<18
neds16 <- neds16 %>% 
  mutate.(AGE = case_when(
    AGE<0 ~ NA_integer_,
    T ~ AGE
  )) 
neds16 <- neds16 %>%
  filter.(AGE>=18)

# Make a list of HOSP_ED with consistent E/M coding
list_of_sites <- neds16 %>%
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
neds16 <- neds16 %>% filter.(HOSP_ED %in% list_of_sites$HOSP_ED)






tic()
core <- fread("../../../../Data/neds/2016_NEDS/NEDS_2016_Core.csv")
colnames(core) <- c("AGE","amonth","aweekend","died_visit","DISCWT","DISP_ED","dqtr","dxver",
                    "edevent","FEMALE","hcupfile","HOSP_ED","DX1","DX2","DX3","DX4","DX5","DX6",
                    "DX7","DX8","DX9","DX10","DX11","DX12","DX13","DX14","DX15","i10_dx16","i10_dx17",
                    "i10_dx18","i10_dx19","i10_dx20","i10_dx21","i10_dx22","i10_dx23","i10_dx24",
                    "i10_dx25","i10_dx26","i10_dx27","i10_dx28","i10_dx29","i10_dx30","i10_ecause1",
                    "i10_ecause2","i10_ecause3","i10_ecause4","i10_ndx","i10_necause","KEY_ED","NEDS_STR",
                    "PAY1","pay2","pl_nchs","TOTCHG_ED","year","ZIPINC_QRTL")
core <- core %>% select.(KEY_ED,DX1:DX15)
toc()

neds16 <- neds16 %>%
  left_join.(core,by="KEY_ED")
rm(core)
gc()




# Tack on diagnosis codes and generate Elixhauser Comorbidity Index
# https://github.com/ellessenne/comorbidity

tic()
diag_list <- neds16 %>%
  #  slice(1:1600000) %>%
  select.(KEY_ED, DX1:DX15) %>%
  pivot_longer.(DX1:DX15) %>%
  rename(code=value) %>%
  select.(-name) %>%
  data.frame()
toc()

tic()
# https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf
library(comorbidity)
elix <- comorbidity(x=diag_list,id="KEY_ED",code="code",score="elixhauser",icd="icd10",assign0=FALSE) %>%
  select.(KEY_ED,score) %>%
  rename.(ELIX = score) %>%
  mutate.(ELIX = case_when(
    ELIX<0 ~ 0,
    T ~ ELIX))
rm(diag_list)
gc()
toc()

neds16 <- neds16 %>%
  left_join.(elix,by="KEY_ED")
rm(elix)
gc()

# Save the analysis sample
saveRDS(neds16,"data-analysis/neds16-analysis.rds")

# Save a stratified subsample
library(splitstackshape)
neds16_strat <- stratified(neds16, c("HOSP_ED"), 0.1)
saveRDS(neds16_strat,"data-analysis/neds16-analysis-subsample.rds")

# Save the hospital-level data
data <- fread("../../../../Data/neds/2016_NEDS/NEDS_2016_Hospital.csv")
colnames(data) <- c("DISCWT","HOSPWT","CONTROL","HOSP_ED","REGION","TRAUMA",
                    "URCAT4","UR_TEACH","NEDS_STR","N_DISC_U","N_HOSP_U",
                    "S_DISC_U","S_HOSP_U","EDVISITS","YEAR")

hosp16 <- list_of_sites %>% left_join.(data,by="HOSP_ED")


saveRDS(hosp16,"data-analysis/hosp16-analysis.rds")