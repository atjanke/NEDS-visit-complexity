# Load full NEDS RDS file
tic()
neds18 <- readRDS("data-cleaned/neds18.rds")
toc()

# Remove age<18
neds18 <- neds18 %>% 
  mutate.(AGE = case_when(
    AGE<0 ~ NA_integer_,
    T ~ AGE
  )) 
neds18 <- neds18 %>%
  filter.(AGE>=18)

# Make a list of HOSP_ED with consistent E/M coding
list_of_sites <- neds18 %>%
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
neds18 <- neds18 %>% filter.(HOSP_ED %in% list_of_sites$HOSP_ED)




# Tack on diagnosis codes and generate Elixhauser Comorbidity Index
# https://github.com/ellessenne/comorbidity

tic()
core <- fread("../../../../Data/neds/2018_NEDS/NEDS_2018_Core.csv")
colnames(core) <- c("AGE","amonth","aweekend","died_visit","DISCWT","DISP_ED","dqtr",
                    "edevent","FEMALE","hcupfile","HOSP_ED","DX1","DX2","DX3","DX4","DX5","DX6",
                    "DX7","DX8","DX9","DX10","DX11","DX12","DX13","DX14","DX15","i10_dx16","i10_dx17",
                    "i10_dx18","i10_dx19","i10_dx20","i10_dx21","i10_dx22","i10_dx23","i10_dx24",
                    "i10_dx25","i10_dx26","i10_dx27","i10_dx28","i10_dx29","i10_dx30",
                    "i10_dx31","i10_dx32","i10_dx33","i10_dx34","i10_dx35",
                    "i10_injury","i10_injury_cut","I10_injury_drown","i10_injury_fall","i10_injury_fire",
                    "i10_injury_firearm","i10_injury_machinery","i10_injury_mvt","i10_injury_nature",
                    "i10_injury_overexertion","i10_injury_poison","i10_injury_struck","i10_injury_suffocation",
                    "i10_intent_assault","i10_intent_self_harm",
                    "i10_intent_unintentional","i10_multinjury","i10_ndx","KEY_ED","NEDS_STR",
                    "PAY1","pay2","pl_nchs","TOTCHG_ED","year","ZIPINC_QRTL")
core <- core %>% select.(KEY_ED,DX1:DX15)
toc()







neds18 <- neds18 %>%
  left_join.(core,by="KEY_ED")
rm(core)
gc()

tic()
diag_list <- neds18 %>%
  select.(KEY_ED, DX1:DX15) %>%
  pivot_longer.(DX1:DX15) %>%
  rename(code=value) %>%
  select.(-name) %>%
  data.frame()
toc()

tic()
# https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf
library(comorbidity)
elix <- comorbidity(x=diag_list,id="KEY_ED",code="code",score="elixhauser",icd="icd10",assign0=TRUE) %>%
  select.(KEY_ED,score) %>%
  rename.(ELIX = score) %>%
  mutate.(ELIX = case_when(
    ELIX<0 ~ 0,
    T ~ ELIX))
rm(diag_list)
gc()
toc()

neds18 <- neds18 %>%
  left_join.(elix,by="KEY_ED")
rm(elix)
gc()




# Save the analysis sample
saveRDS(neds18,"data-analysis/neds18-analysis.rds")

# Save a stratified subsample
library(splitstackshape)
neds18_strat <- stratified(neds18, c("HOSP_ED"), 0.1)
saveRDS(neds18_strat,"data-analysis/neds18-analysis-subsample.rds")



# Save the hospital-level data
data <- fread("../../../../Data/neds/2018_NEDS/NEDS_2018_Hospital.csv")
colnames(data) <- c("DISCWT","HOSPWT","CONTROL","HOSP_ED","REGION","TRAUMA",
                    "URCAT4","UR_TEACH","NEDS_STR","N_DISC_U","N_HOSP_U",
                    "S_DISC_U","S_HOSP_U","EDVISITS","YEAR")

hosp18 <- list_of_sites %>% left_join.(data,by="HOSP_ED")


saveRDS(hosp18,"data-analysis/hosp18-analysis.rds")
