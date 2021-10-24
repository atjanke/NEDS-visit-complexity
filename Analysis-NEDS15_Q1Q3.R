setwd("/Users/alexanderjanke/Data/neds/2015_NEDS")

diag <- fread("NEDS_2015Q1Q3_ED.csv")

colnames(diag) <- c(
  "chron1","chron2","chron3","chron4","chron5","chron6","chron7","chron8","chron9","chron10","chron11","chron12",
  "chron13","chron14","chron15","chron16","chron17","chron18","chron19","chron20","chron21","chron22","chron23",
  "chron24","chron25","chron26","chron27","chron28","chron29","chron30","CPT1","CPT2","CPT3","CPT4","CPT5","CPT6",
  "CPT7","CPT8","CPT9","CPT10","CPT11","CPT12","CPT13","CPT14","CPT15","PRCCSED1","PRCCSED2","PRCCSED3","PRCCSED4","PRCCSED5",
  "PRCCSED6","PRCCSED7","PRCCSED8","PRCCSED9","PRCCSED10","PRCCSED11","PRCCSED12","PRCCSED13","PRCCSED14","PRCCSED15","DX1","DX2",
  "DX3","DX4","DX5","DX6","DX7","DX8","DX9","DX10","DX11","DX12","DX13","DX14","DX15","DX16","DX17","DX18","DX19","DX20",
  "DX21","DX22","DX23","DX24","DX25","DX26","DX27","DX28","DX29","DX30","DXCCS1","DXCCS2","DXCCS3","DXCCS4","DXCCS5",
  "DXCCS6","DXCCS7","DXCCS8","DXCCS9","DXCCS10","DXCCS11","DXCCS12","DXCCS13","DXCCS14","DXCCS15","DXCCS16","DXCCS17",
  "DXCCS18","DXCCS19","DXCCS20","DXCCS21","DXCCS22","DXCCS23","DXCCS24","DXCCS25","DXCCS26","DXCCS27","DXCCS28","DXCCS29",
  "DXCCS30","dxver","ecode1","ecode2","ecode3","ecode4","e_ccs1","e_ccs2","e_ccs3","e_ccs4","hcupfile","hosp_ed","injury",
  "injury_cut","injury_drown","injury_fall","injury_fire","injury_firearm","injury_machinery","injury_mvt","injury_nature",
  "injury_poison","injury_severity","injury_struck","injury_suffocation","intent_assault","intent_self_harm","intent_unintentional",
  "KEY_ED","multinjury","ncpt","ndx","necode")

diag <- diag %>% select.(KEY_ED,DX1:DX15)

neds15_Q1Q3 <- neds15_Q1Q3 %>%
  left_join.(diag,by="KEY_ED")

rm(diag)
gc()

# Tack on diagnosis codes and generate Elixhauser Comorbidity Index
# https://github.com/ellessenne/comorbidity

tic()
diag_list <- neds15_Q1Q3 %>%
  #  slice(1:1500000) %>%
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

neds15_Q1Q3 <- neds15_Q1Q3 %>%
  left_join.(elix,by="KEY_ED")
rm(elix)
gc()