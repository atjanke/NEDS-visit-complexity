#### LIBRARIES                                          ####
library(tidyverse)
library(data.table)
library(tidytable)
library(tidymodels)
library(stringr)
library(scales)
library(readxl)

#### LOAD CPT CODE LABELS                               ####
CPT_Labels <- read_excel("../../../Codes/CPT-Labels.xlsx")

#### LOAD DATA                                          ####
neds <- readRDS("__data-analysis/neds19-analysis.rds")

#### NUMBER OF UNIQUE CPT CODES                         ####
unique_cpt <- neds %>% select.(CPT1:CPT15) %>%
  pivot_longer.() %>% select.(value) %>%
  unique() %>% 
            # Remove the blanks
            filter.(value!="" & 
            # Remove the E/M codes
            str_detect(value,"9928")==FALSE & 
            str_detect(value,"9929")==FALSE)

#### MOST COMMON CCSR CODES                             ####
CCSR_Codes_Frequency <- neds %>%
  select.(DX1) %>%
  left_join.(readRDS("../../../Codes/ccsr.rds") %>% select.(-ICD10_Description) %>% rename(DX1=ICD10),by="DX1") %>%
  summarise.(Proportion=n.()/nrow(neds),CCSR_Description=first(CCSR_Description),.by="CCSR") %>%
  arrange.(-Proportion) %>%
  mutate.(Proportion=percent(Proportion))

CCSR_For_Model <- CCSR_Codes_Frequency %>% slice(1:25)

#### MOST COMMON CPT CODES                              ####
CPT_Codes_Frequency <- neds %>%
  select.(CPT1:CPT15) %>%
  pivot_longer.() %>%
  select.(value) %>%
  filter.(value %in% unique_cpt$value) %>%
  summarise.(Proportion=(n.()/nrow(neds)),.by="value") %>%
  rename.(CPT=value) %>%
  arrange.(-Proportion) %>%
  mutate.(Proportion=percent(Proportion)) %>%
  left_join.(CPT_Labels,by="CPT")

CPT_For_Model <- CPT_Codes_Frequency %>% slice(1:25)

#######################################################
################BUILD DATA FOR MODEL###################
#######################################################
#### Slice data                                         ####
df <- neds %>%
  sample_n(100000)

#### Create indicator variables for top 25 CCSR codes   ####
df <- df %>%
  left_join.(readRDS("../../../Codes/ccsr.rds") %>% select.(-ICD10_Description) %>% rename(DX1=ICD10),.by="DX1") %>%
  mutate.(CCSR=case_when(CCSR %in% CCSR_For_Model$CCSR~CCSR,T~"")) %>%
  mutate.(CCSR=factor(CCSR))

#### Create indicator variables for top 25 CPT codes    ####
CPT_Indicators <- df %>%
  select.(KEY_ED,CPT1:CPT15) %>%
  pivot_longer.(cols=c(CPT1:CPT15)) %>%
  select.(-name) %>%
  rename.(CPT=value) %>%
  left_join.(CPT_For_Model %>% mutate(Indicator=1) ,by="CPT") %>%
  filter.(Indicator==1) %>%
  unique() %>%
  pivot_wider.(id_cols=KEY_ED,names_from=CPT,values_from=Indicator)
colnames(CPT_Indicators)[2:26] <- paste0("CPT",colnames(CPT_Indicators)[2:26])

# Join the indicators with df
df <- df %>%
  left_join.(CPT_Indicators,by="KEY_ED") %>%
  mutate.(High_Intens=ifelse(
    EM_Code=="99285"|EM_Code=="99291",1,0)) %>%
  select.(High_Intens,AGE,FEMALE,PAY1,ELIX,starts_with("CPT")) %>%
  select.(-c(CPT1:CPT15))

# Replace NAs in CPT columns with 0
df[is.na(df)]<-0

#### Prepare df for modeling                            ####
df <- df %>%
  mutate.(High_Intens=factor(High_Intens)) %>%
  mutate.(AGE=factor(cut(AGE,c(17,45,55,65,75,85,120)))) %>%
  mutate.(FEMALE=factor(case_when(FEMALE==1~1,T~0))) %>%
  mutate.(PAY1=factor(case_when(
    PAY1==1~"Medicare",
    PAY1==2~"Medicaid",
    PAY1==3~"Private Insurance",
    PAY1==4~"Self-Pay",
    T~"Other"))) %>%
  mutate.(ELIX=factor(case_when(ELIX==0~"0",ELIX==1~"1",ELIX==2~"2",T~"3+")))

#######################################################
######################MODELING#########################
#######################################################
#### Estimate model                                  ####
logistic_model <- logistic_reg() %>%
  set_engine("glm") %>% set_mode("classification") %>%
  fit(High_Intens~.,data=df)

#### Summarize model coefficients                    ####
output <- tidy(logistic_model,exponentiate=TRUE,conf.int=TRUE) %>%
  mutate(term=case_when(
    str_detect(term,"CPT")~substr(term,4,8),T~term)) %>%
  left_join(CPT_Labels %>% select(CPT,CPT_Description) %>% rename(term=CPT),by="term") %>%
  select("term","CPT_Description","estimate","conf.low","conf.high","p.value")

#######################################################