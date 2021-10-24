library(data.table)
library(tidytable)
library(dplyr)
library(tidyr)
library(tidymodels)
library(parallel)
library(tictoc)

cores <- detectCores()-3

tic(msg="Load the neds06-analysis dataset")
neds06 <- readRDS("data-analysis/neds06-analysis.rds")
toc()

##############
# PREPARE DATA

tic(msg="Prepare data for model building")
df <- neds06 %>% 
  filter.(EM_Code!="Not listed") %>%
  sample_n(200000) %>%
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
    DX1:DX15,
    CPT1:CPT15) %>%
  mutate(rowname=paste0("Row",rep(1:n())))

ICD <- df %>%
  select(DX1:DX15) %>%
  mutate(rowname=paste0("Row",rep(1:n()))) %>%
  pivot_longer(cols=-rowname) %>%
  select(-name) %>%
  filter(value!=-999) %>%
  mutate(
    Constant=1
  ) %>%
  unique() %>%
  filter(value!="")
ICD_List <- ICD %>%
  group_by(value) %>%
  summarise(Count=n()) %>%
  filter(Count>999) %>%
  select(value) %>%
  rename(Code=value)
ICD <- ICD %>%
  filter(value %in% ICD_List$Code) %>%
  pivot_wider(names_from=value,names_prefix="ICD_",values_from=Constant) %>%
  mutate_all(~ifelse(is.na(.),0,.))
colnames(ICD)[1] <- "rowname"

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
  filter(Count>999) %>%
  select(value) %>%
  rename(Code=value)
CPT <- CPT %>%
  filter(value %in% CPT_List$Code) %>%
  pivot_wider(names_from=value,names_prefix="CPT",values_from=Constant) %>%
  mutate_all(~ifelse(is.na(.),0,.))
colnames(CPT)[1] <- "rowname"

df <- df %>%  
  select.(High_Intensity_Billing,AGE) %>%
  mutate(rowname=paste0("Row",rep(1:n()))) %>%
  left_join(CPT,by="rowname")
df <- df %>%  select(-rowname)
df <- df %>%  mutate_all(~ifelse(is.na(.),0,.))
rm(ICD,ICD_List,CPT,CPT_List)

df <- df %>% mutate.(High_Intensity_Billing=factor(High_Intensity_Billing))
df$High_Intensity_Billing <- relevel(df$High_Intensity_Billing,ref="99281-99284")
toc()

model_elements <- colnames(df)
print("Number of model elements (ICD/CPT groups 1000+ rows in sample")
print(ncol(df)-1)
saveRDS(model_elements,"data-analysis/model_elements.rds")

df_split <- initial_split(df,prop=0.75,strata=High_Intensity_Billing)

df_train <- df_split %>% training()
df_test  <- df_split %>% testing()






########
performance_table <- data.frame()

########
# # LOGISTIC REGRESSION MODEL
tic(msg="Train the logistic regression model")
logistic_model <- logistic_reg() %>%
  set_engine("glm") %>% set_mode("classification") %>%
  fit(High_Intensity_Billing~.,data=df_train)
toc()
saveRDS(logistic_model, "data-analysis/logistic06.rds")

table_logistic_coefficients <- tidy(logistic_model, exponentiate=TRUE)
write.csv(table_logistic_coefficients,"Results/Logistic-Coefficients.csv")

logistic_results <- df_test %>%
  select(High_Intensity_Billing) %>%
  bind_cols(predict(logistic_model,new_data=df_test,type="class"),
            predict(logistic_model,new_data=df_test,type="prob"))

print("Predicted number of high-intensity visits in the hold out set: ") #6435
a<-colSums(logistic_results[,4])
a
print("Observed number of high-intensity visits in the hold out set: ")  #6467
b<-nrow(df_test[High_Intensity_Billing=="High-Intensity"])
b
print("Percent difference: ")
1- a/b

logistic_conf_mat <- conf_mat (logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)
saveRDS(logistic_conf_mat,"Results/Logistic-Confusion-Matrix.rds")

performance_table[1,1] <- accuracy (logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,2] <- sens     (logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,3] <- spec     (logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,4] <- precision(logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[1,5] <- recall   (logistic_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]

# Fix column names for AUROC
colnames(logistic_results) <- c("High_Intensity_Billing",".pred_class","pred_High_Intes","pred9928")
performance_table[1,6] <- roc_auc  (logistic_results, truth = High_Intensity_Billing,estimate = pred_High_Intes)[3]

colnames(performance_table) <- c("accuracy","sens","spec","precision","recall","auroc")

performance_table <- performance_table %>% mutate(Model = "Logistic Regression")











########
# # RANDOM FOREST MODEL
tic(msg="Train the random forest model")
rf_model <- rand_forest(mode="classification") %>%
  set_engine("ranger",num.threads=cores) %>%
  fit(High_Intensity_Billing~.,data=df_train)
toc()
saveRDS(rf_model, "data-analysis/forest06.rds")

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

rf_conf_mat <- conf_mat (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)
saveRDS(rf_conf_mat,"Results/Random-Forest-Confusion-Matrix.rds")

performance_table[2,1] <- accuracy (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[2,2] <- sens     (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[2,3] <- spec     (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[2,4] <- precision(rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
performance_table[2,5] <- recall   (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]

# Fix column names for AUROC
colnames(rf_results)       <- c("High_Intensity_Billing",".pred_class","pred_High_Intes","pred9928")

performance_table[2,6] <- roc_auc  (rf_results, truth = High_Intensity_Billing,estimate = pred_High_Intes)[3]

performance_table <- performance_table %>% mutate(Model = case_when(
                            is.na(Model) ~ "Random Forest", T ~ Model))

write.csv(performance_table,"Results/Model-Performance-Table.csv")





########
# # AUROC Figure
p<-roc_curve(logistic_results,High_Intensity_Billing,pred_High_Intes) %>%
  mutate(Model="Logistic Regression") %>%
  rbind(
    roc_curve(rf_results,High_Intensity_Billing,pred_High_Intes) %>% mutate(Model="Random Forest")
  ) %>%
  ggplot(aes(x=1-specificity,y=sensitivity,color=Model))+
  geom_path()+
  geom_abline(lty=3)+
  coord_equal()+
  theme_bw()
p
ggsave("Results/AUROC.tiff",p,dpi=300)