library(data.table)
library(tidytable)
library(dplyr)
library(tidyr)
library(tidymodels)
library(parallel)
library(tictoc)
library(ggplot2)

library(stringr)

ccs <- fread("ccs.csv") %>% select(c(1:4))
colnames(ccs) <- c("ICD9","CCS","CCS_Description","ICD9_Description")

ccs <-  ccs %>%
  mutate.(ICD9 = str_replace_all(ICD9, "[[:punct:]]", ""),
          CCS  = as.numeric(str_replace_all(CCS, "[[:punct:]]", "")),
          CCS_Description = 
            str_replace_all(CCS_Description,"[[:punct:]]", "")) 


ccsr <- fread("ccsr.csv") %>% select(c(1:4))
colnames(ccsr) <- c("ICD10","ICD10_Description","CCSR","CCSR_Description")

ccsr <- ccsr %>%
  mutate.(ICD10=  str_replace_all(ICD10,"[[:punct:]]", ""),
          ICD10_Description=  str_replace_all(ICD10_Description,"[[:punct:]]", ""),
          CCSR=  str_replace_all(CCSR,"[[:punct:]]", ""),
          CCSR_Description=  str_replace_all(CCSR_Description,"[[:punct:]]", ""))






df <- fread("descriptive-output/CCS-Counts-Proportions.csv") %>%
  select.(-V1) %>%
  mutate.(CCS=as.numeric(CCS))


largest_gain_2006_2014 <- df %>%
  mutate.(Gain = Proportion_6/Proportion_14) %>%
  arrange.(-Gain) %>%
  select.(CCS, Gain, Proportion_14) %>%
  slice(1:25) %>%
  left_join(
    
    ccs %>% group_by(CCS) %>% summarise(CCS_Description=first(CCS_Description))
    ,by="CCS"
  )



df <- fread("descriptive-output/CCS-Counts-Proportions.csv") %>%
  select.(-V1)

largest_gain_2016_2018 <- df %>%
  mutate.(Gain = Proportion_16/Proportion_18) %>%
  arrange.(-Gain) %>%
  select.(CCS,Gain,Proportion_18) %>%
  rename(CCSR=CCS) %>%
  slice(1:25) %>%
  left_join(
    
    ccsr %>% group_by(CCSR) %>% summarise(CCSR_Description=first(CCSR_Description))
    ,by="CCSR"
  )