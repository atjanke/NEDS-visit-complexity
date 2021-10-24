library(dplyr)
library(data.table)
library(tidyverse)

# Load full NEDS RDS file
tic()
neds06 <- readRDS("data-cleaned/neds06.rds")
toc()

# Remove age<18
neds06 <- neds06 %>% 
  mutate.(AGE = case_when(
    AGE<0 ~ NA_integer_,
    T ~ AGE
  )) 
neds06 <- neds06 %>%
  filter.(AGE>=18)

# Make a list of HOSP_ED with consistent E/M coding
list_of_sites <- neds06 %>%
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
neds06 <- neds06 %>% filter.(HOSP_ED %in% list_of_sites$HOSP_ED)


# Tack on diagnosis codes and generate Elixhauser Comorbidity Index
# https://github.com/ellessenne/comorbidity

tic()
diag_list <- neds06 %>%
#  slice(1:1000000) %>%
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

neds06 <- neds06 %>%
  left_join.(elix,by="KEY_ED")
rm(elix)
gc()

# Save the analysis sample
saveRDS(neds06,"data-analysis/neds06-analysis.rds")

# Save a stratified subsample
library(splitstackshape)
neds06_strat <- stratified(neds06, c("HOSP_ED"), 0.1)
saveRDS(neds06_strat,"data-analysis/neds06-analysis-subsample.rds")

# Save the hospital-level data
data <- fread("../../../../Data/neds/2006_NEDS/NEDS_2006_Hospital.csv")
colnames(data) <- c("DISCWT","HOSPWT","CONTROL","HOSP_ED","REGION","TRAUMA",
                      "URCAT4","UR_TEACH","NEDS_STR","N_DISC_U","N_HOSP_U",
                      "S_DISC_U","S_HOSP_U","EDVISITS","YEAR")

hosp06 <- list_of_sites %>% left_join.(data,by="HOSP_ED")


saveRDS(hosp06,"data-analysis/hosp06-analysis.rds")