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
################################ LOGISTIC ################################
##########################################################################
#### Set parameters                                                    ####
# YEAR_=2013
# DATA_SIZE_=100000

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

tic(msg="Prepare data for model building")
# If we are using 2015 data for estimating the model, limit to Q4 (ICD-10)
if (input_==15) {neds <- neds %>% filter(ICD=="10")}

# Fix columns names in the 2016 data
if (input_>=16) {
  colnames(neds)[49:(49+14)] <-
    c("DXCCS1","DXCCS2","DXCCS3","DXCCS4","DXCCS5","DXCCS6","DXCCS7","DXCCS8","DXCCS9","DXCCS10",
      "DXCCS11","DXCCS12","DXCCS13","DXCCS14","DXCCS15")}

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
    PAY1,
    ELIX,
    FEMALE,
    DXCCS1:DXCCS15,
    CPT1:CPT15,
    DISP_ED) %>%
  mutate.(PAY1=factor(PAY1)) %>%
  mutate.(ELIX=factor(ELIX)) %>%
  mutate(rowname=paste0("Row",rep(1:n())))

df <- df %>% mutate.(FEMALE=ifelse(FEMALE==1,1,0)) %>%
  mutate.(FEMALE=factor(FEMALE))

df <- df %>%
  mutate.(AGE=cut(AGE,c(17,44,54,64,74,84,120))) %>%
  mutate.(AGE_1 = ifelse(AGE=="(17,44]",1,0), 
          AGE_2 = ifelse(AGE=="(44,54]",1,0), 
          AGE_3 = ifelse(AGE=="(54,64]",1,0),
          AGE_4 = ifelse(AGE=="(64,74]",1,0), 
          AGE_5 = ifelse(AGE=="(74,84]",1,0),
          AGE_6 = ifelse(AGE=="(84,120]",1,0))

CCS <- df %>%
  select(DXCCS1:DXCCS15) %>%
  mutate(rowname=paste0("Row",rep(1:n()))) %>%
  pivot_longer(cols=-rowname) %>%
  select(-name) %>%
  filter(value!=-999 & value!=-99 & value!=-100) %>%
  mutate(
    Constant=1
  ) %>%
  unique() %>%
  filter(value!="")
CCS_List <- CCS %>%
  group_by(value) %>%
  summarise(Count=n()) %>%
  filter(Count>5000) %>%
  select(value) %>%
  rename(Code=value)
CCS <- CCS %>%
  filter(value %in% CCS_List$Code) %>%
  pivot_wider(names_from=value,names_prefix="CCS",values_from=Constant) %>%
  mutate_all(~ifelse(is.na(.),0,.))
colnames(CCS)[1] <- "rowname"

CPT <- df %>%
  select(CPT1:CPT15) %>%
  mutate(rowname=paste0("Row",rep(1:n()))) %>%
  pivot_longer(cols=-rowname) %>%
  select(-name) %>%
  filter(value!="invl"  & value!="" & 
           value!="99281" & value!="99282" & value!="99283" & value!="99284" & value!="99285" & value!="99291") %>%
  mutate(
    Constant=1
  ) %>%
  unique()
CPT_List <- CPT %>%
  group_by(value) %>%
  summarise(Count=n()) %>%
  filter(Count>5000) %>%
  select(value) %>%
  rename(Code=value)
CPT <- CPT %>%
  filter(value %in% CPT_List$Code) %>%
  pivot_wider(names_from=value,names_prefix="CPT",values_from=Constant) %>%
  mutate_all(~ifelse(is.na(.),0,.))
colnames(CPT)[1] <- "rowname"

df <- df %>%  
  select.(High_Intensity_Billing,AGE,AGE_1,AGE_2,AGE_3,AGE_4,AGE_5,AGE_6,PAY1,ELIX,DISP_ED) %>%
  mutate(rowname=paste0("Row",rep(1:n()))) %>%
  left_join(CCS,by="rowname") %>%
  left_join(CPT,by="rowname")
df <- df %>%  select(-rowname)
df <- df %>%  mutate_all(~ifelse(is.na(.),0,.))
rm(CCS,CCS_List,CPT,CPT_List)

df <- df %>% mutate.(High_Intensity_Billing=factor(High_Intensity_Billing))
df$High_Intensity_Billing <- relevel(df$High_Intensity_Billing,ref="99281-99284")
toc()

# Remove problematic  column
if ("CCSXXX000" %in% colnames(df)) { 
  df <- df %>%
  select.(-CCSXXX000)
}

model_elements <- colnames(df)
print("Number of model elements (ICD/CPT groups 1000+ rows in sample")
print(ncol(df)-1)

df_split <- initial_split(df,prop=0.75,strata=High_Intensity_Billing)

df_train <- df_split %>% training()
df_test  <- df_split %>% testing()
rm(df_split)


#### Estimate the models                                               ####
tic(msg="Train the logistic regression model")
logistic_model <- logistic_reg() %>%
  set_engine("glm") %>% set_mode("classification") %>%
  fit(High_Intensity_Billing~.,data=df_train)
toc()

# tic(msg="Train the random forest model")
# rf_model <- rand_forest(mode="classification",trees=500) %>%
#   set_engine("ranger",num.threads=cores,importance="permutation") %>%
#   fit(High_Intensity_Billing~.,data=df_train)
# toc()

#### Implement the model in subsequent year's data                     ####
input_  <- YEAR_-2000 +1
if (input_< 10) {
  neds <- readRDS(paste0("__data-analysis/neds0",input_,"-analysis.rds"))}
if (input_>=10) {
  neds <- readRDS(paste0("__data-analysis/neds",input_,"-analysis.rds"))}
toc()

if (input_<15) {neds <- neds %>% mutate(ICD="9")}
if (input_>15) {neds <- neds %>% mutate(ICD="10")}

tic(msg="Prepare data for model building")
# If we are applying the model in 2015, limit to Q1-Q3, ICD-9 data
if (input_==15) {neds <- neds %>% filter(ICD=="9")}

# Fix columns names in the 2016 data
if (input_>=16) {
  colnames(neds)[49:(49+14)] <-
    c("DXCCS1","DXCCS2","DXCCS3","DXCCS4","DXCCS5","DXCCS6","DXCCS7","DXCCS8","DXCCS9","DXCCS10",
      "DXCCS11","DXCCS12","DXCCS13","DXCCS14","DXCCS15")
}

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
    PAY1,
    ELIX,
    FEMALE,
    DXCCS1:DXCCS15,
    CPT1:CPT15,
    DISP_ED) %>%
  mutate.(PAY1=factor(PAY1)) %>%
  mutate.(ELIX=factor(ELIX)) %>%
  mutate(rowname=paste0("Row",rep(1:n())))

df <- df %>% mutate.(FEMALE=ifelse(FEMALE==1,1,0)) %>%
  mutate.(FEMALE=factor(FEMALE))

df <- df %>%
  mutate.(AGE=cut(AGE,c(17,44,54,64,74,84,120))) %>%
  mutate.(AGE_1 = ifelse(AGE=="(17,44]",1,0), 
          AGE_2 = ifelse(AGE=="(44,54]",1,0), 
          AGE_3 = ifelse(AGE=="(54,64]",1,0),
          AGE_4 = ifelse(AGE=="(64,74]",1,0), 
          AGE_5 = ifelse(AGE=="(74,84]",1,0),
          AGE_6 = ifelse(AGE=="(84,120]",1,0))

CCS <- df %>%
  select(DXCCS1:DXCCS15) %>%
  mutate(rowname=paste0("Row",rep(1:n()))) %>%
  pivot_longer(cols=-rowname) %>%
  select(-name) %>%
  filter(value!=-100 & value!=-99) %>%
  mutate(
    Constant=1
  ) %>%
  unique() %>%
  filter(value!="")
CCS_List <- CCS %>%
  group_by(value) %>%
  summarise(Count=n()) %>%
  filter(Count>100) %>%
  select(value) %>%
  rename(Code=value)
CCS <- CCS %>%
  filter(value %in% CCS_List$Code) %>%
  pivot_wider(names_from=value,names_prefix="CCS",values_from=Constant) %>%
  mutate_all(~ifelse(is.na(.),0,.))
colnames(CCS)[1] <- "rowname"

CPT <- df %>%
  select(CPT1:CPT15) %>%
  mutate(rowname=paste0("Row",rep(1:n()))) %>%
  pivot_longer(cols=-rowname) %>%
  select(-name) %>%
  filter(value!="invl"  & value!="" & 
           value!="99281" & value!="99282" & value!="99283" & value!="99284" & value!="99285" & value!="99291") %>%
  mutate(
    Constant=1
  ) %>%
  unique()
CPT_List <- CPT %>%
  group_by(value) %>%
  summarise(Count=n()) %>%
  filter(Count>100) %>%
  select(value) %>%
  rename(Code=value)
CPT <- CPT %>%
  filter(value %in% CPT_List$Code) %>%
  pivot_wider(names_from=value,names_prefix="CPT",values_from=Constant) %>%
  mutate_all(~ifelse(is.na(.),0,.))
colnames(CPT)[1] <- "rowname"

df <- df %>%  
  select.(High_Intensity_Billing,AGE, AGE_1,AGE_2,AGE_3,AGE_4,AGE_5,AGE_6,ELIX,PAY1,DISP_ED) %>%
  mutate(rowname=paste0("Row",rep(1:n()))) %>%
  left_join(CCS,by="rowname") %>%
  left_join(CPT,by="rowname")
df <- df %>%  select(-rowname)
df <- df %>%  mutate_all(~ifelse(is.na(.),0,.))
rm(CCS,CCS_List,CPT,CPT_List)

df <- df %>% mutate.(High_Intensity_Billing=factor(High_Intensity_Billing))
df$High_Intensity_Billing <- relevel(df$High_Intensity_Billing,ref="99281-99284")

list_of_missing_columns <- as.list(setdiff(model_elements,colnames(df)))

df_append <- data.frame(list_of_missing_columns) %>% 
  rename_all(~gsub(x = .,pattern = "X.",replacement = "")) %>% 
  rename_all(~gsub(x = .,pattern = ".$",replacement = "")) %>% 
  mutate_all(~0)

df <- df %>%
  cbind(df_append)
rm(df_append)
df <- df %>%
  select.(model_elements)

pred_class_logistic  <- predict(logistic_model,new_data=df,type="class")
pred_prob_logistic   <- predict(logistic_model,new_data=df,type="prob")

results_logistic <- df %>%
  select(High_Intensity_Billing) %>%
  bind_cols(pred_class_logistic,pred_prob_logistic)

estimated_logistic <- floor(sum(results_logistic[,4]))
observed_logistic <- nrow(results_logistic[results_logistic$High_Intensity_Billing=="High-Intensity",])
proportion_obs_logistic <- round(observed_logistic/nrow(results_logistic),3)
proportion_est_logistic <- round(estimated_logistic/nrow(results_logistic),3)

print(paste0("Estimated high-intensity billing count: ",estimated_logistic))
print(paste0("Observed high-intensity billing count: ", observed_logistic))
print(paste0("Rate: ",proportion_est_logistic," | expected high-intensity billing"))
print(paste0("Rate: ",proportion_obs_logistic," | observed high-intensity billing"))

#### Summarize logistic regression model performance                   ####
logistic_results <- df_test %>%
  select(High_Intensity_Billing) %>%
  bind_cols(predict(logistic_model,new_data=df_test,type="class"),
            predict(logistic_model,new_data=df_test,type="prob"))

print("Predicted number of high-intensity visits in the hold out set: ")
a_logistic<-colSums(logistic_results[,4])
a_logistic
print("Observed number of high-intensity visits in the hold out set: ")
b_logistic<-nrow(df_test[High_Intensity_Billing=="High-Intensity"])
b_logistic
print("Percent difference: ")
1- a_logistic/b_logistic

logistic_conf_mat <- conf_mat (logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)

performance_table <- data.frame()
performance_table[1,1] <- accuracy (logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,2] <- sens     (logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,3] <- spec     (logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,4] <- precision(logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,5] <- recall   (logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,6] <- 1-a_logistic/b_logistic

# Fix column names for AUROC
colnames(logistic_results) <- c("High_Intensity_Billing",".pred_class","pred_High_Intes","pred9928")
performance_table[1,7] <- roc_auc  (logistic_results, truth = High_Intensity_Billing,estimate = pred_High_Intes)[3]

performance_table[1,8] <- a_logistic
performance_table[1,9] <- b_logistic
performance_table[1,10] <- proportion_est_logistic
performance_table[1,11] <- proportion_obs_logistic
performance_table[1,12] <- nrow(df_train)
performance_table[1,13] <- nrow(df_test)
performance_table[1,14] <- nrow(df)
performance_table[1,15] <- ncol(df)

colnames(performance_table) <- c("accuracy","sens","spec","precision","recall","test.set.diff","auroc",
                                 "pred.hold.out","obs.hold.out",
                                 "pred.subseq.year","observed.subseq.year",
                                 "n.train","n.test","n.implement","n.parameters")
performance_table <- performance_table %>% 
  mutate(Model = "Logistic Regression") %>%
  mutate(Year = YEAR_)

saveRDS(performance_table,paste0("02_Results/Logistic-",YEAR_,"-Performance.rds"))

# 
# 
# pred_class_rf  <- predict(rf_model,new_data=df,type="class")
# pred_prob_rf   <- predict(rf_model,new_data=df,type="prob")
# 
# results_rf<- df %>%
#   select(High_Intensity_Billing) %>%
#   bind_cols(pred_class_rf,pred_prob_rf)
# 
# estimated_rf <- floor(sum(results_rf[,4]))
# observed_rf <- nrow(results_rf[results_rf$High_Intensity_Billing=="High-Intensity",])
# proportion_obs_rf <- round(observed_rf/nrow(results_rf),3)
# proportion_est_rf <- round(estimated_rf/nrow(results_rf),3)
# 
# print(paste0("Estimated high-intensity billing count: ",estimated_rf))
# print(paste0("Observed high-intensity billing count: ", observed_rf))
# print(paste0("Rate: ",proportion_est_rf," | expected high-intensity billing"))
# print(paste0("Rate: ",proportion_obs_rf," | observed high-intensity billing"))

#### #### Summarize random forest model performance                    ####
# rf_results <- df_test %>%
#   select(High_Intensity_Billing) %>%
#   bind_cols(predict(rf_model,new_data=df_test,type="class"),
#             predict(rf_model,new_data=df_test,type="prob"))
# 
# print("Predicted number of high-intensity visits in the hold out set: ")
# a_rf<-colSums(rf_results[,4])
# a_rf
# print("Observed number of high-intensity visits in the hold out set: ")
# b_rf<-nrow(df_test[High_Intensity_Billing=="High-Intensity"])
# b_rf
# print("Percent difference: ")
# 1- a_rf/b_rf
# 
# rf_conf_mat <- conf_mat (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)
# 
# performance_table[2,1] <- accuracy (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
# performance_table[2,2] <- sens     (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
# performance_table[2,3] <- spec     (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
# performance_table[2,4] <- precision(rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
# performance_table[2,5] <- recall   (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
# performance_table[2,6] <- 1-a_rf/b_rf
# 
# # Fix column names for AUROC
# colnames(rf_results) <- c("High_Intensity_Billing",".pred_class","pred_High_Intes","pred9928")
# performance_table[2,7] <- roc_auc  (rf_results, truth = High_Intensity_Billing,estimate = pred_High_Intes)[3]
# 
# performance_table[2,8] <- a_rf
# performance_table[2,9] <- b_rf
# performance_table[2,10] <- proportion_est_rf
# performance_table[2,11] <- proportion_obs_rf
# performance_table[2,12] <- nrow(df_train)
# performance_table[2,13] <- nrow(df_test)
# performance_table[2,14] <- nrow(df)
# performance_table[2,15] <- ncol(df)
# 
# 
# performance_table <- performance_table %>% 
#   mutate(Model = case_when(is.na(Model)~"Random Forest",T~Model)) %>%
#   mutate(Year = YEAR_)
# 
# saveRDS(performance_table,paste0("02_Results/",YEAR_,"-Performance.rds"))

#### Output model elements importance for logistic regression          ####
ccs <- readRDS("../../../Codes/ccs.rds") %>% rename(Label=CCS) %>% mutate(Label=as.character(Label)) %>%
  select(Label, CCS_Description) %>%
  unique()
cpt <- read_excel("../../../Codes/CPT-Labels.xlsx",sheet="Labels") %>% rename(Label=CPT) %>% mutate(Label=as.character(Label))

output <- tidy(logistic_model,exponentiate=TRUE) %>%
  mutate(Label = case_when(
    str_detect(term,"CCS")~substr(term,4,6),
    str_detect(term,"CPT")~substr(term,4,8))) %>%
  left_join(ccs,by="Label") %>%
  left_join(cpt,by="Label") %>%
  mutate(Description = case_when(
    is.na(CCS_Description) ~ CPT_Description,
    T ~ CCS_Description)) %>%
  select(-CPT_Description,-CCS_Description,-Label)

saveRDS(output,paste0("02_Results/Logistic-",YEAR_,"-Coefficients.rds"))
#### Clean  memory                                                     ####
rm(list = ls())
gc()