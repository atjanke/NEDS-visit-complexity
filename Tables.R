#### Libraries            ####
library(tidyverse)
library(data.table)
library(tidytable)
library(tidymodels)
library(tictoc)
library(parallel)
library(gridExtra)
library(scales)
library(stringr)
library(readxl)


#### Table 1              ####
table <- data.frame()
for (i in 6:19) {
  index_ = i-5
  if (i<10) {
    read_ <- paste0("__data-analysis/neds0",i,"-analysis.rds")
  }
  if (i>=10) {
    read_ <- paste0("__data-analysis/neds",i,"-analysis.rds")
  }
  
  neds <- readRDS(read_)
  
  neds <- neds %>%
    mutate.(Age_Category = case_when(
      AGE>=18 & AGE<45 ~ "18-44",
      AGE>=45 & AGE<65 ~ "45-64",
      AGE>=65 & AGE<85 ~ "65-84",
      AGE>=85          ~ "85+")) %>%
    mutate.(ELIX = case_when(
      ELIX>2 ~ 2, T ~ ELIX))
  
  
  
  table[1,index_] <- nrow(neds)
  
  table[2,index_] <- nrow(neds[neds$PAY1==3])/nrow(neds)
  table[3,index_] <- nrow(neds[neds$PAY1==1])/nrow(neds)
  table[4,index_] <- nrow(neds[neds$PAY1==2])/nrow(neds)
  table[5,index_] <- nrow(neds[neds$PAY1!=3 & neds$PAY1!=1 & neds$PAY1!=2 & neds$PAY1!=4])/nrow(neds)
  table[6,index_] <- nrow(neds[neds$PAY1==4])/nrow(neds)
  
  table[7,index_] <- nrow(neds[neds$Age_Category=="18-44"])/nrow(neds)
  table[8,index_] <- nrow(neds[neds$Age_Category=="45-64"])/nrow(neds)
  table[9,index_] <- nrow(neds[neds$Age_Category=="65-84"])/nrow(neds)
  table[10,index_] <- nrow(neds[neds$Age_Category=="85+"])/nrow(neds)
  
  table[11,index_] <- nrow(neds[neds$ELIX==0])/nrow(neds)
  table[12,index_] <- nrow(neds[neds$ELIX==1])/nrow(neds)
  table[13,index_] <- nrow(neds[neds$ELIX==2])/nrow(neds)
  
  table[14,index_] <- nrow(neds[neds$DISP_ED==1])/nrow(neds)
  table[15,index_] <- nrow(neds[neds$DISP_ED==5])/nrow(neds)
  table[16,index_] <- nrow(neds[neds$DISP_ED==6])/nrow(neds)
  table[17,index_] <- (nrow(neds[neds$DISP_ED!=6 & neds$DISP_ED!=5 & neds$DISP_ED!=1]))/nrow(neds)
}

table[1,] <- lapply(table[1,], round, 0)
table[2:17,] <- lapply(table[2:17,],percent,accuracy=0.1)

write.csv(table,"03_Tables/Table-1-Raw-Output.csv")


#### Table 2              ####
table <- data.frame()
for (i in 6:19) {
  index_ = i-5
  if (i<10) {
    read_ <- paste0("__data-analysis/neds0",i,"-analysis.rds")
  }
  if (i>=10) {
    read_ <- paste0("__data-analysis/neds",i,"-analysis.rds")
  }
  
  neds <- readRDS(read_)
  
  neds <- neds %>%
    mutate.(High_Intensity_Billing = ifelse(
      EM_Code=="99285" | EM_Code=="99291",1,0)) %>%
    mutate.(Insurance = case_when(
      PAY1==1 ~ "Medicare",
      PAY1==2 ~ "Medicaid",
      PAY1==3 ~ "Private insurance",
      PAY1==4 ~ "Self-pay",
      T ~ "Other")) %>%
    mutate.(Age_Category = case_when(
      AGE>=18 & AGE<45 ~ "18-44",
      AGE>=45 & AGE<65 ~ "45-64",
      AGE>=65 & AGE<85 ~ "65-84",
      AGE>=85          ~ "85+")) %>%
    mutate.(Disposition = case_when(
      DISP_ED==1 ~ "Routine",
      DISP_ED==2 ~ "Transfer to short term",
      DISP_ED==5 ~ "Transfer to skilled",
      DISP_ED==6 ~ "Home Health Care"
    ))
  
  table[1,index_] <- nrow(neds[neds$High_Intensity_Billing==1])/nrow(neds)
  table[2,index_] <- nrow(neds[neds$EM_Code=="99291"])/nrow(neds)
  table[3,index_] <- nrow(neds[neds$EM_Code=="99285"])/nrow(neds)
  table[4,index_] <- nrow(neds[neds$EM_Code=="99284"])/nrow(neds)
  table[5,index_] <- nrow(neds[neds$EM_Code=="99283"])/nrow(neds)
  table[6,index_] <- nrow(neds[neds$EM_Code=="99282"])/nrow(neds)
  table[7,index_] <- nrow(neds[neds$EM_Code=="99281"])/nrow(neds)
  
  table[8,index_] <-  nrow(neds[neds$Insurance=="Private insurance" & neds$High_Intensity_Billing==1])/nrow(neds[neds$Insurance=="Private insurance"])
  table[9,index_] <-  nrow(neds[neds$Insurance=="Medicare"          & neds$High_Intensity_Billing==1])/nrow(neds[neds$Insurance=="Medicare"])
  table[10,index_] <- nrow(neds[neds$Insurance=="Medicaid"          & neds$High_Intensity_Billing==1])/nrow(neds[neds$Insurance=="Medicaid"])
  table[11,index_] <- nrow(neds[neds$Insurance=="Other"             & neds$High_Intensity_Billing==1])/nrow(neds[neds$Insurance=="Other"])
  table[12,index_] <- nrow(neds[neds$Insurance=="Self-pay"          & neds$High_Intensity_Billing==1])/nrow(neds[neds$Insurance=="Self-pay"])
  
  table[13,index_] <- nrow(neds[neds$Age_Category=="18-44"            & neds$High_Intensity_Billing==1])/nrow(neds[neds$Age_Category=="18-44"])
  table[14,index_] <- nrow(neds[neds$Age_Category=="45-64"            & neds$High_Intensity_Billing==1])/nrow(neds[neds$Age_Category=="45-64"])
  table[15,index_] <- nrow(neds[neds$Age_Category=="65-84"            & neds$High_Intensity_Billing==1])/nrow(neds[neds$Age_Category=="65-84"])
  table[16,index_] <- nrow(neds[neds$Age_Category=="85+"              & neds$High_Intensity_Billing==1])/nrow(neds[neds$Age_Category=="85+"])
  
  table[17,index_] <- nrow(neds[neds$ELIX==0 & neds$High_Intensity_Billing==1])/nrow(neds[neds$ELIX==0])
  table[18,index_] <- nrow(neds[neds$ELIX==1 & neds$High_Intensity_Billing==1])/nrow(neds[neds$ELIX==1])
  table[19,index_] <- nrow(neds[neds$ELIX>=2 & neds$High_Intensity_Billing==1])/nrow(neds[neds$ELIX>=2])
  
  
  table[20,index_] <- nrow(neds[neds$Disposition=="Routine"                    & neds$High_Intensity_Billing==1])/nrow(neds[neds$Disposition=="Routine"])
  table[21,index_] <- nrow(neds[neds$Disposition=="Transfer to skilled"        & neds$High_Intensity_Billing==1])/nrow(neds[neds$Disposition=="Transfer to skilled"])
  table[22,index_] <- nrow(neds[neds$Disposition=="Home Health Care"           & neds$High_Intensity_Billing==1])/nrow(neds[neds$Disposition=="Home Health Care"])
  table[23,index_] <- nrow(neds[neds$Disposition!="Routine" & neds$Disposition!="Transfer to skilled" & neds$Disposition!="Home Health Care"  & neds$High_Intensity_Billing==1])/nrow(neds[neds$Disposition!="Routine" & neds$Disposition!="Transfer to skilled" & neds$Disposition!="Home Health Care"])
  
}

table <- lapply(table,percent,accuracy=0.1)
write.csv(table,"03_Tables/Table-2-Raw-Output.csv")


#### Table 3              ####

neds06 <- readRDS("__data-analysis/neds06-analysis.rds")
neds19 <- readRDS("__data-analysis/neds19-analysis.rds")

ccs  <- readRDS("../../../Codes/ccs.RDS")
ccsr <- readRDS("../../../Codes/ccsr.RDS") %>% rename(DX1=ICD10)

table <- cbind(
    neds06 %>% 
      summarise.(
        Proportion = n.()/nrow(neds06),
        .by="DXCCS1") %>%
      arrange.(-Proportion) %>%
      mutate.(Total = paste0(substr(percent(round(cumsum(Proportion),5)),1,4),"%")) %>%
      mutate.(Proportion=paste0(substr(percent(round(Proportion,5)),1,4),"%")) %>%
      slice.(1:20) %>%
      rename.(CCS=DXCCS1) %>%
      left_join.(
        ccs %>% group_by(CCS) %>% summarise(CCS_Description=first(CCS_Description)),
        by="CCS") %>%
      select.(CCS_Description,Proportion,Total),
    
    neds19 %>% 
      left_join.(ccsr,by="DX1") %>%
      summarise.(
        Proportion = n.()/nrow(neds19),
        .by="CCSR") %>%
      arrange.(-Proportion) %>%
      mutate.(Total = paste0(substr(percent(round(cumsum(Proportion),5)),1,4),"%")) %>%
      mutate.(Proportion=paste0(substr(percent(round(Proportion,5)),1,4),"%")) %>%
      slice.(1:20) %>%
      left_join.(
        ccsr %>% group_by(CCSR) %>% summarise(CCSR_Description=first(CCSR_Description)),
        by="CCSR") %>%
      select.(CCSR_Description,Proportion,Total))

write.csv(table,"03_Tables/Table-3-Raw-Output.csv")

#### Table 4              ####

neds06 <- readRDS("__data-analysis/neds06-analysis-subsample.rds")
neds19 <- readRDS("__data-analysis/neds19-analysis-subsample.rds")

CPT <- read_excel("../../../Codes/CPT-Labels.xlsx") %>% select(CPT,CPT_Description)

table <- cbind(
    neds06 %>%
      select.(CPT1:CPT15) %>%
      pivot_longer.(CPT1:CPT15) %>%
      filter.(value!="invl" & value != "" &
                value!="99281" &
                value!="99282" &
                value!="99283" &
                value!="99284" &
                value!="99285" &
                value!="99291") %>%
      summarise.(
        Proportion = n.()/nrow(neds06),
        .by="value") %>%
      arrange.(-Proportion) %>%
      mutate.(Proportion=paste0(substr(percent(round(Proportion,5)),1,4),"%")) %>%
      slice.(1:20) %>%
      rename.(CPT=value) %>%
      left_join.(CPT,by="CPT") %>% select.(CPT,CPT_Description,Proportion),
    neds19 %>%
      select.(CPT1:CPT15) %>%
      pivot_longer.(CPT1:CPT15) %>%
      filter.(value!="invl" & value != "" &
                value!="99281" &
                value!="99282" &
                value!="99283" &
                value!="99284" &
                value!="99285" &
                value!="99291") %>%
      summarise.(
        Proportion = n.()/nrow(neds19),
        .by="value") %>%
      arrange.(-Proportion) %>%
      mutate.(Proportion=paste0(substr(percent(round(Proportion,5)),1,4),"%")) %>%
      slice.(1:20) %>%
      rename.(CPT=value) %>%
  left_join.(CPT,by="CPT")  %>% select.(CPT,CPT_Description,Proportion))
write.csv(table,"03_Tables/Table-4-Raw-Output.csv")


#### Supplemental Table 1 ####
table <- data.frame()

for (i in 6:19) {
  index_ = i-5
  if (i<10) {
    read_ <- paste0("__data-analysis/hosp0",i,"-analysis.rds")
    read_original <- paste0("../../../../Data/neds/200",i,"_NEDS/NEDS_200",i,"_Hospital.csv")
  }
  if (i>=10) {
    read_ <- paste0("__data-analysis/hosp",i,"-analysis.rds")
    read_original <- paste0("../../../../Data/neds/20",i,"_NEDS/NEDS_20",i,"_Hospital.csv")
  }
  hosp <- readRDS(read_) %>%
    mutate.(Region = case_when(
      REGION==1 ~ "Northeast",
      REGION==2 ~ "Midwest",
      REGION==3 ~ "South",
      REGION==4 ~ "West"
    )) %>%
    mutate.(Urbanicity = case_when(
      URCAT4==1 ~ "Large Metro",
      URCAT4==3 ~ "Micro",
      T ~ "Small Metro"
    )) %>%
    mutate.(Visits = case_when(
      EDVISITS<20000 ~ "<20,000",
      EDVISITS>=20000 & EDVISITS<40000 ~ "20-40,000",
      EDVISITS>=40000 & EDVISITS<60000 ~ "40-60,000",
      EDVISITS>=60000 & EDVISITS<80000 ~ "60-80,000",
      EDVISITS>=80000                  ~ "80,000+"))
  
  hosp_original <- fread(read_original)
    
  colnames(hosp_original) <- c("DISCWT","HOSPWT","CONTROL","HOSP_ED","REGION","TRAUMA",
                               "URCAT4","UR_TEACH","NEDS_STR","N_DISC_U","N_HOSP_U",
                               "S_DISC_U","S_HOSP_U","EDVISITS","YEAR")
  
  hosp_original <- hosp_original %>%
    mutate.(Region = case_when(
      REGION==1 ~ "Northeast",
      REGION==2 ~ "Midwest",
      REGION==3 ~ "South",
      REGION==4 ~ "West"
    )) %>%
    mutate.(Urbanicity = case_when(
      URCAT4==1 ~ "Large Metro",
      URCAT4==3 ~ "Micro",
      T ~ "Small Metro"
    )) %>%
    mutate.(Visits = case_when(
      EDVISITS<20000 ~ "<20,000",
      EDVISITS>=20000 & EDVISITS<40000 ~ "20-40,000",
      EDVISITS>=40000 & EDVISITS<60000 ~ "40-60,000",
      EDVISITS>=60000 & EDVISITS<80000 ~ "60-80,000",
      EDVISITS>=80000                  ~ "80,000+"))
  
  index_=index_*2-1
  
  table[1,index_] <- nrow(hosp_original)
  
  table[2,index_] <- nrow(hosp_original[hosp_original$Region=="Northeast"])/nrow(hosp_original)
  table[3,index_] <- nrow(hosp_original[hosp_original$Region=="Midwest"])/nrow(hosp_original)
  table[4,index_] <- nrow(hosp_original[hosp_original$Region=="South"])/nrow(hosp_original)
  table[5,index_] <- nrow(hosp_original[hosp_original$Region=="West"])/nrow(hosp_original)
  
  table[6,index_] <- nrow(hosp_original[hosp_original$Urbanicity=="Large Metro"])/nrow(hosp_original)
  table[7,index_] <- nrow(hosp_original[hosp_original$Urbanicity=="Small Metro"])/nrow(hosp_original)
  table[8,index_] <- nrow(hosp_original[hosp_original$Urbanicity=="Micro"])/nrow(hosp_original)
  
  table[9,index_] <- nrow(hosp_original[hosp_original$Visits=="<20,000"])/nrow(hosp_original)
  table[10,index_] <- nrow(hosp_original[hosp_original$Visits=="20-40,000"])/nrow(hosp_original)
  table[11,index_] <- nrow(hosp_original[hosp_original$Visits=="40-60,000"])/nrow(hosp_original)
  table[12,index_] <- nrow(hosp_original[hosp_original$Visits=="60-80,000"])/nrow(hosp_original)
  table[13,index_] <- nrow(hosp_original[hosp_original$Visits=="80,000+"])/nrow(hosp_original)

  table[1,index_+1] <- nrow(hosp)
  
  table[2,index_+1] <- nrow(hosp[hosp$Region=="Northeast"])/nrow(hosp)
  table[3,index_+1] <- nrow(hosp[hosp$Region=="Midwest"])/nrow(hosp)
  table[4,index_+1] <- nrow(hosp[hosp$Region=="South"])/nrow(hosp)
  table[5,index_+1] <- nrow(hosp[hosp$Region=="West"])/nrow(hosp)
  
  table[6,index_+1] <- nrow(hosp[hosp$Urbanicity=="Large Metro"])/nrow(hosp)
  table[7,index_+1] <- nrow(hosp[hosp$Urbanicity=="Small Metro"])/nrow(hosp)
  table[8,index_+1] <- nrow(hosp[hosp$Urbanicity=="Micro"])/nrow(hosp)
  
  table[9,index_+1] <- nrow(hosp[hosp$Visits=="<20,000"])/nrow(hosp)
  table[10,index_+1] <- nrow(hosp[hosp$Visits=="20-40,000"])/nrow(hosp)
  table[11,index_+1] <- nrow(hosp[hosp$Visits=="40-60,000"])/nrow(hosp)
  table[12,index_+1] <- nrow(hosp[hosp$Visits=="60-80,000"])/nrow(hosp)
  table[13,index_+1] <- nrow(hosp[hosp$Visits=="80,000+"])/nrow(hosp)
}

table[1,] <- lapply(table[1,], round, 0)
table[2:13,] <- lapply(table[2:13,],percent,accuracy=0.1)

write.csv(table,"03_Tables/Supplemental-Table-1-Raw-Output.csv")
####