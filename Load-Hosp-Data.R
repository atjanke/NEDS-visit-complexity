#### Libraries    ####
library(tidyverse)
library(tidytable)
library(tidymodels)
library(tictoc)
library(parallel)
library(gridExtra)
library(scales)

for (i in 6:19) {
  if (i< 10) {
    data_ <- paste0("../../../../Data/neds/200",i,"_NEDS/NEDS_200",i,"_Hospital.csv")
  
    data <- fread(data_)
    colnames(data) <- c("DISCWT","HOSPWT","CONTROL","HOSP_ED","REGION","TRAUMA",
                        "URCAT4","UR_TEACH","NEDS_STR","N_DISC_U","N_HOSP_U",
                        "S_DISC_U","S_HOSP_U","EDVISITS","YEAR")
    
    save_ <- paste0("__data-cleaned/hosp0",i,".rds")
    saveRDS(data,save_)
  }
  
  if (i>=10) {
    data_ <- paste0("../../../../Data/neds/20",i,"_NEDS/NEDS_20",i,"_Hospital.csv")
  
    data <- fread(data_)
    colnames(data) <- c("DISCWT","HOSPWT","CONTROL","HOSP_ED","REGION","TRAUMA",
                      "URCAT4","UR_TEACH","NEDS_STR","N_DISC_U","N_HOSP_U",
                      "S_DISC_U","S_HOSP_U","EDVISITS","YEAR")
  
    save_ <- paste0("__data-cleaned/hosp",i,".rds")
    saveRDS(data,save_)
  }
}
  
  
  
