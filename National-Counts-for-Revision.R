library(data.table)
library(tidytable)
library(tidyverse)
library(splitstackshape)

neds06_file <- "/Users/alexanderjanke/Data/neds/2006_NEDS/NEDS_2006_CORE.csv"
core06_columns <- c("AGE","AMONTH","AWEEKEND","CHRON1","CHRON2","CHRON3","CHRON4","CHRON5","CHRON6",
                    "CHRON7","CHRON8","CHRON9","CHRON10","CHRON11","CHRON12","CHRON13","CHRON14","CHRON15",
                    "DIED_VIS","DISCWT","DISP_ED","DQTR","DX1","DX2","DX3","DX4","DX5","DX6","DX7","DX8",
                    "DX9","DX10","DX11","DX12","DX13","DX14","DX15","DXCCS1","DXCCS2","DXCCS3","DXCCS4",
                    "DXCCS5","DXCCS6","DXCCS7","DXCCS8","DXCCS9","DXCCS10","DXCCS11","DXCCS12","DXCCS13",
                    "DXCCS14","DXCCS15","ECODE1","ECODE2","ECODE3","ECODE4","EDEVENT","E_CCS1","E_CCS2",
                    "E_CCS3","E_CCS4","FEMALE","HCUPFILE","HOSP_ED","REGION","INTENT_S","KEY_ED","NDX",
                    "NECODE","NEDS_STR","PAY1","PAY2","PL_NCHS2","TOTCHGED","YEAR","ZIPINC_Q")

neds06 <- fread(neds06_file,
                #nrow=100000,
                select=c(20,21)) %>%
  setNames(c("DISCWT","DISP_ED")) %>%
  mutate.(treat.and.release = DISP_ED==9) %>%
  summarise.(Visits.Nationally = sum(DISCWT),.by="treat.and.release")

neds19_file <- "/Users/alexanderjanke/Data/neds/2019_NEDS/NEDS_2019_CORE.csv"
core19_columns <- c("AGE","amonth","aweekend","died_visit","DISCWT","DISP_ED","dqtr",
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
                    "PAY1","pay2","pl_nchs","TOTCHG_ED","year","race","ZIPINC_QRTL")
neds19 <- fread(neds19_file,
                #nrow=10000,
                select=c(5,6)) %>%
  setNames(c("DISCWT","DISP_ED")) %>%
  mutate.(treat.and.release = DISP_ED==9) %>%
  summarise.(Visits.Nationally = sum(DISCWT),.by="treat.and.release")



neds06 %>% colSums()
