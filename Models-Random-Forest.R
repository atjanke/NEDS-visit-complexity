#### Libraries                                                         ####
library(data.table)
library(tidytable)
library(dplyr)
library(tidyr)
library(tidymodels)
library(parallel)
library(tictoc)
library(readxl)
library(stringr)
library(forcats)
library(vip)
cores <- detectCores()-3
performance_table <- data.frame()

##########################################################################
############################### RANDOM FOREST ############################
##########################################################################
#### Set parameters                                                    ####
YEAR_=2015
DATA_SIZE_=500000
cores <- detectCores()-3
#### Build df_test/df_train for model input                            ####
tic(msg="Load the neds-analysis dataset")
input_  <- YEAR_-2000
if (input_< 10) {
  neds <- readRDS(paste0("__data-analysis/neds0",input_,"-analysis.rds"))}
if (input_>=10) {
  neds <- readRDS(paste0("__data-analysis/neds",input_,"-analysis.rds"))
  if (input_==15) {neds <- readRDS(paste0("__data-analysis/neds",input_,"-analysis.rds"))}
}
toc()

if (input_<15) {neds <- neds %>% mutate(ICD="9")}
if (input_>15) {neds <- neds %>% mutate(ICD="10")}

# Fix columns names in the 2016+ data
if (input_>=16) {
  colnames(neds)[49:(49+14)] <-
    c("DXCCS1","DXCCS2","DXCCS3","DXCCS4","DXCCS5","DXCCS6","DXCCS7","DXCCS8","DXCCS9","DXCCS10",
      "DXCCS11","DXCCS12","DXCCS13","DXCCS14","DXCCS15")}

tic(msg="Prepare data for model building")
# If we are using 2015 data for estimating the model, limit to Q4 (ICD-10)
if (input_==15) {neds <- neds %>% filter(ICD=="10")}
# Create dataframe for model building
df <- neds %>% 
  filter.(EM_Code!="Not listed") %>%
  sample_n(DATA_SIZE_) %>%
  mutate.(
    EM_Code = factor(EM_Code,levels=c("99281","99282","99283","99284","99285","99291"))) %>%
  mutate.(High_Intensity_Billing = case_when(
    EM_Code=="99291" | EM_Code=="99285" ~ "High-Intensity",
    T ~ "99281-99284"
  )) %>%
  select.(
    High_Intensity_Billing,
    AGE,
    FEMALE,
    DXCCS1:DXCCS15,
    CPT1:CPT15)

df <- df %>% mutate.(High_Intensity_Billing=factor(High_Intensity_Billing))
df$High_Intensity_Billing <- relevel(df$High_Intensity_Billing,ref="99281-99284")
df <- df %>% mutate.(FEMALE=ifelse(FEMALE==1,1,0)) %>%
  mutate.(FEMALE=factor(FEMALE))

df <- df %>%
  # Hide the E/M codes from the model
  mutate(across(starts_with("CPT"),~case_when(
    .=="99281"|.=="99282"|.=="99283"|.=="99284"|.=="99285"|.=="99291"~"",T~.))) 
df <- df %>%
  # Convert CCS/CPT codes to factor variables
  mutate(across(starts_with("DXCCS"),factor)) %>%
  mutate(across(starts_with("CPT"),factor))

df <- df %>%
  mutate(across(starts_with("DXCCS"),~factor(.,levels=c(levels(.),"None")))) %>%
  mutate(across(starts_with("CPT"),~factor(.,levels=c(levels(.),"None"))))

print("The rate of high-intensity billing in the modeled year: ")
df %>% mutate(Indicator=ifelse(High_Intensity_Billing!="99281-99284",1,0)) %>%
  summarise(Proportion=sum(Indicator)/n()) %>% print()

df_split <- initial_split(df,prop=0.75,strata=High_Intensity_Billing)

df_train <- df_split %>% training()
df_test  <- df_split %>% testing()

#### Estimate a random forest model                                    ####
rf_model <- rand_forest(mode="classification",trees=100,min_n=10000) %>%
  set_engine("ranger",num.threads=cores,importance="permutation") %>%
  fit(High_Intensity_Billing~.,data=df_train)
#### Implement the model in the subsequent year's data                 ####
df_original <- df
input_  <- YEAR_-2000 +1
if (input_< 10) {
  neds <- readRDS(paste0("__data-analysis/neds0",input_,"-analysis.rds"))}
if (input_>=10) {
  neds <- readRDS(paste0("__data-analysis/neds",input_,"-analysis.rds"))}
toc()

if (input_<15) {neds <- neds %>% mutate(ICD=="9")}
if (input_>15) {neds <- neds %>% mutate(ICD=="10")}

# Fix columns names in the 2016 data
if (input_>=16) {
  colnames(neds)[49:(49+14)] <-
    c("DXCCS1","DXCCS2","DXCCS3","DXCCS4","DXCCS5","DXCCS6","DXCCS7","DXCCS8","DXCCS9","DXCCS10",
      "DXCCS11","DXCCS12","DXCCS13","DXCCS14","DXCCS15")}

tic(msg="Prepare data for model building")
# If we are applying the model in 2015, limit to Q1-Q3, ICD-9 data
if (input_==2015) {neds <- neds %>% filter(ICD=="9")}

df <- neds %>% 
  filter.(EM_Code!="Not listed") %>%
  sample_n(DATA_SIZE_) %>%
  mutate.(
    EM_Code = factor(EM_Code,levels=c("99281","99282","99283","99284","99285","99291"))) %>%
  mutate.(High_Intensity_Billing = case_when(
    EM_Code=="99291" | EM_Code=="99285" ~ "High-Intensity",
    T ~ "99281-99284"
  )) %>%
  select.(
    High_Intensity_Billing,
    AGE,
    FEMALE,
    DXCCS1:DXCCS15,
    CPT1:CPT15)

df <- df %>% mutate.(High_Intensity_Billing=factor(High_Intensity_Billing))
df$High_Intensity_Billing <- relevel(df$High_Intensity_Billing,ref="99281-99284")
df <- df %>% mutate.(FEMALE=ifelse(FEMALE==1,1,0)) %>%
  mutate.(FEMALE=factor(FEMALE))

df <- df %>%
  # Hide the E/M codes from the model
  mutate(across(starts_with("CPT"),~ifelse(
    .=="99281"|.=="99282"|.=="99283"|.=="99284"|.=="99285"|.=="99291","",.)))

df <- df %>%
  mutate(across(starts_with("DXCCS"),factor)) %>%
  mutate(across(starts_with("CPT"),factor))

#### ... convert factor levels for subsequent year data                ####
df$DXCCS1 <-  factor(df$DXCCS1,  levels=levels(df_original$DXCCS1))
df$DXCCS2 <-  factor(df$DXCCS2,  levels=levels(df_original$DXCCS2))
df$DXCCS3 <-  factor(df$DXCCS3,  levels=levels(df_original$DXCCS3))
df$DXCCS4 <-  factor(df$DXCCS4,  levels=levels(df_original$DXCCS4))
df$DXCCS5 <-  factor(df$DXCCS5,  levels=levels(df_original$DXCCS5))
df$DXCCS6 <-  factor(df$DXCCS6,  levels=levels(df_original$DXCCS6))
df$DXCCS7 <-  factor(df$DXCCS7,  levels=levels(df_original$DXCCS7))
df$DXCCS8 <-  factor(df$DXCCS8,  levels=levels(df_original$DXCCS8))
df$DXCCS9 <-  factor(df$DXCCS9,  levels=levels(df_original$DXCCS9))
df$DXCCS10 <- factor(df$DXCCS10, levels=levels(df_original$DXCCS10))
df$DXCCS11 <- factor(df$DXCCS11, levels=levels(df_original$DXCCS11))
df$DXCCS12 <- factor(df$DXCCS12, levels=levels(df_original$DXCCS12))
df$DXCCS13 <- factor(df$DXCCS13, levels=levels(df_original$DXCCS13))
df$DXCCS14 <- factor(df$DXCCS14, levels=levels(df_original$DXCCS14))
df$DXCCS15 <- factor(df$DXCCS15, levels=levels(df_original$DXCCS15))
df$CPT1    <- factor(df$CPT1,    levels=(unique(df_original$CPT1)))
df$CPT2    <- factor(df$CPT2,    levels=(unique(df_original$CPT2)))
df$CPT3    <- factor(df$CPT3,    levels=(unique(df_original$CPT3)))
df$CPT4    <- factor(df$CPT4,    levels=(unique(df_original$CPT4)))
df$CPT5    <- factor(df$CPT5,    levels=(unique(df_original$CPT5)))
df$CPT6    <- factor(df$CPT6,    levels=(unique(df_original$CPT6)))
df$CPT7    <- factor(df$CPT7,    levels=(unique(df_original$CPT7)))
df$CPT8    <- factor(df$CPT8,    levels=(unique(df_original$CPT8)))
df$CPT9    <- factor(df$CPT9,    levels=(unique(df_original$CPT9)))
df$CPT10   <- factor(df$CPT10,   levels=(unique(df_original$CPT10)))
df$CPT11   <- factor(df$CPT11,   levels=(unique(df_original$CPT11)))
df$CPT12   <- factor(df$CPT12,   levels=(unique(df_original$CPT12)))
df$CPT13   <- factor(df$CPT13,   levels=(unique(df_original$CPT13)))
df$CPT14   <- factor(df$CPT14,   levels=(unique(df_original$CPT14)))
df$CPT15   <- factor(df$CPT15,   levels=(unique(df_original$CPT15)))

df <- df %>%
  mutate(across(starts_with("DXCCS"), ~fct_explicit_na(.,"None"))) %>%
  mutate(across(starts_with("CPT"), ~fct_explicit_na(.,"None")))

#### ... continued                                                     ####
pred_class  <- predict(rf_model,new_data=df,type="class")
pred_prob   <- predict(rf_model,new_data=df,type="prob")

results <- df %>%
  select(High_Intensity_Billing) %>%
  bind_cols(pred_class,pred_prob)

estimated <- floor(sum(results[,4]))
observed <- nrow(results[results$High_Intensity_Billing=="High-Intensity",])
proportion_obs <- round(observed/nrow(results),3)
proportion_est <- round(estimated/nrow(results),3)

print(paste0("Estimated high-intensity billing count: ",estimated))
print(paste0("Observed high-intensity billing count: ", observed))
print(paste0("Rate: ",proportion_est," | expected high-intensity billing"))
print(paste0("Rate: ",proportion_obs," | observed high-intensity billing"))

#### Summarize model performance                                       ####
df_test$DXCCS1 <-  factor(df_test$DXCCS1,  levels=levels(df_original$DXCCS1))
df_test$DXCCS2 <-  factor(df_test$DXCCS2,  levels=levels(df_original$DXCCS2))
df_test$DXCCS3 <-  factor(df_test$DXCCS3,  levels=levels(df_original$DXCCS3))
df_test$DXCCS4 <-  factor(df_test$DXCCS4,  levels=levels(df_original$DXCCS4))
df_test$DXCCS5 <-  factor(df_test$DXCCS5,  levels=levels(df_original$DXCCS5))
df_test$DXCCS6 <-  factor(df_test$DXCCS6,  levels=levels(df_original$DXCCS6))
df_test$DXCCS7 <-  factor(df_test$DXCCS7,  levels=levels(df_original$DXCCS7))
df_test$DXCCS8 <-  factor(df_test$DXCCS8,  levels=levels(df_original$DXCCS8))
df_test$DXCCS9 <-  factor(df_test$DXCCS9,  levels=levels(df_original$DXCCS9))
df_test$DXCCS10 <- factor(df_test$DXCCS10, levels=levels(df_original$DXCCS10))
df_test$DXCCS11 <- factor(df_test$DXCCS11, levels=levels(df_original$DXCCS11))
df_test$DXCCS12 <- factor(df_test$DXCCS12, levels=levels(df_original$DXCCS12))
df_test$DXCCS13 <- factor(df_test$DXCCS13, levels=levels(df_original$DXCCS13))
df_test$DXCCS14 <- factor(df_test$DXCCS14, levels=levels(df_original$DXCCS14))
df_test$DXCCS15 <- factor(df_test$DXCCS15, levels=levels(df_original$DXCCS15))
df_test$CPT1    <- factor(df_test$CPT1,    levels=(unique(df_original$CPT1)))
df_test$CPT2    <- factor(df_test$CPT2,    levels=(unique(df_original$CPT2)))
df_test$CPT3    <- factor(df_test$CPT3,    levels=(unique(df_original$CPT3)))
df_test$CPT4    <- factor(df_test$CPT4,    levels=(unique(df_original$CPT4)))
df_test$CPT5    <- factor(df_test$CPT5,    levels=(unique(df_original$CPT5)))
df_test$CPT6    <- factor(df_test$CPT6,    levels=(unique(df_original$CPT6)))
df_test$CPT7    <- factor(df_test$CPT7,    levels=(unique(df_original$CPT7)))
df_test$CPT8    <- factor(df_test$CPT8,    levels=(unique(df_original$CPT8)))
df_test$CPT9    <- factor(df_test$CPT9,    levels=(unique(df_original$CPT9)))
df_test$CPT10   <- factor(df_test$CPT10,   levels=(unique(df_original$CPT10)))
df_test$CPT11   <- factor(df_test$CPT11,   levels=(unique(df_original$CPT11)))
df_test$CPT12   <- factor(df_test$CPT12,   levels=(unique(df_original$CPT12)))
df_test$CPT13   <- factor(df_test$CPT13,   levels=(unique(df_original$CPT13)))
df_test$CPT14   <- factor(df_test$CPT14,   levels=(unique(df_original$CPT14)))
df_test$CPT15   <- factor(df_test$CPT15,   levels=(unique(df_original$CPT15)))

df_test <- df_test %>%
  mutate(across(starts_with("DXCCS"), ~fct_explicit_na(.,"None"))) %>%
  mutate(across(starts_with("CPT"), ~fct_explicit_na(.,"None")))


rf_results <- df_test %>%
  select(High_Intensity_Billing) %>%
  bind_cols(predict(rf_model,new_data=df_test,type="class"),
            predict(rf_model,new_data=df_test,type="prob"))

print("Predicted number of high-intensity visits in the hold out set: ")
a<-colSums(rf_results[,4])
a
print("Observed number of high-intensity visits in the hold out set: ")
b<-nrow(df_test[High_Intensity_Billing=="High-Intensity"])
b
print("Percent difference: ")
1- a/b

random_forest_conf_mat <- conf_mat (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)

performance_table <- data.frame()
performance_table[1,1] <- accuracy (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,2] <- sens     (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,3] <- spec     (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,4] <- precision(rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,5] <- recall   (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,6] <- 1-a/b

# Fix column names for AUROC
colnames(rf_results) <- c("High_Intensity_Billing",".pred_class","pred_High_Intes","pred9928")
performance_table[1,7] <- roc_auc  (rf_results, truth = High_Intensity_Billing,estimate = pred_High_Intes)[3]

performance_table[1,8] <- a
performance_table[1,9] <- b
performance_table[1,10] <- proportion_est
performance_table[1,11] <- proportion_obs
performance_table[1,12] <- nrow(df_train)
performance_table[1,13] <- nrow(df_test)
performance_table[1,14] <- nrow(df)
performance_table[1,15] <- ncol(df)

colnames(performance_table) <- c("accuracy","sens","spec","precision","recall","test.set.diff","auroc",
                                 "pred.hold.out","obs.hold.out",
                                 "pred.subseq.year","observed.subseq.year",
                                 "n.train","n.test","n.implement","n.parameters")
performance_table <- performance_table %>% 
  mutate(Model = "Random Forest") %>%
  mutate(Year = YEAR_)

saveRDS(performance_table,paste0("02_Results/RF-",YEAR_,"-Performance.rds"))

#### Clean  memory                                                     ####
rm(list = ls())
gc()
