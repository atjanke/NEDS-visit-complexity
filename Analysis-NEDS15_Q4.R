setwd("/Users/alexanderjanke/Data/neds/2015_NEDS")

diag <- fread("NEDS_2015Q4_ED.csv")

colnames(diag) <- c("CPT1","CPT2","CPT3","CPT4","CPT5","CPT6",
                    "CPT7","CPT8","CPT9","CPT10","CPT11","CPT12","CPT13","CPT14","CPT15",
                    "PRCCSED1","PRCCSED2","PRCCSED3","PRCCSED4","PRCCSED5",
                    "PRCCSED6","PRCCSED7","PRCCSED8","PRCCSED9","PRCCSED10","PRCCSED11","PRCCSED12",
                    "PRCCSED13","PRCCSED14","PRCCSED15","dxver","hcupfile","hosp_ed","DX1","DX2",
                    "DX3","DX4","DX5","DX6","DX7","DX8","DX9","DX10","DX11","DX12","DX13","DX14",
                    "DX15","i10_dx16","i10_dx17","i10_dx18","i10_dx19","i10_dx20","i10_dx21","i10_dx22",
                    "i10_dx23","i10_dx24","i10_dx25","i10_dx26","i10_dx27","i10_dx28","i10_dx29","i10_dx30",
                    "i10_ecause1","i10_ecause2","i10_ecause3","i10_ecause4","i10_ndx","i10_necause","KEY_ED","NCPT")

diag <- diag %>% select.(KEY_ED,DX1:DX15)

neds15_Q4 <- neds15_Q4 %>%
  left_join.(diag,by="KEY_ED")

rm(diag)
gc()

# Tack on diagnosis codes and generate Elixhauser Comorbidity Index
# https://github.com/ellessenne/comorbidity

tic()
diag_list <- neds15_Q4 %>%
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
elix <- comorbidity(x=diag_list,id="KEY_ED",code="code",score="elixhauser",icd="icd10",assign0=FALSE) %>%
  select.(KEY_ED,score,chf:depre) %>%
  rename.(ELIX = score) %>%
  mutate.(ELIX = case_when(
    ELIX<0 ~ 0,
    T ~ ELIX))
rm(diag_list)
gc()
toc()

neds15_Q4 <- neds15_Q4 %>%
  left_join.(elix,by="KEY_ED")
rm(elix)
gc()