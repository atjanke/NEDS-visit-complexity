neds06 <- readRDS("data-cleaned/neds06.rds")

df <- neds06 %>% 
  filter.(EM_Code!="Not listed") %>%
  slice.(1:50000) %>%
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
    CPT1:CPT15) %>%
  mutate(rowname=paste0("Row",rep(1:n())))

CCS <- df %>%
  select(DXCCS1:DXCCS15) %>%
  mutate(rowname=paste0("Row",rep(1:n()))) %>%
  pivot_longer(cols=-rowname) %>%
  select(-name) %>%
  filter(value!=-999) %>%
  mutate(
    Constant=1
  ) %>%
  unique() %>%
  pivot_wider(names_from=value,names_prefix="CCS",values_from=Constant) %>%
  mutate_all(~ifelse(is.na(.),0,.))
CCS <- 
  cbind(
    CCS$rowname,
    select_if(CCS,is.numeric)[,colSums(select_if(CCS,is.numeric))>1000])
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
  unique() %>%
  pivot_wider(names_from=value,names_prefix="CPT",values_from=Constant) %>%
  mutate_all(~ifelse(is.na(.),0,.))
CPT <- 
  cbind(
    CPT$rowname,
  select_if(CPT,is.numeric)[,colSums(select_if(CPT,is.numeric))>1000])
colnames(CPT)[1] <- "rowname"

df <- df %>%  
  select.(High_Intensity_Billing,AGE) %>%
  mutate(rowname=paste0("Row",rep(1:n()))) %>%
  left_join(CCS,by="rowname") %>%
  left_join(CPT,by="rowname")
df <- df %>%  select(-rowname)
df <- df %>%  mutate_all(~ifelse(is.na(.),0,.))
rm(CCS,CPT)

df <- df %>% mutate.(High_Intensity_Billing=factor(High_Intensity_Billing))
df$High_Intensity_Billing <- relevel(df$High_Intensity_Billing,ref="High-Intensity")

library(tidymodels)

df_split <- initial_split(df,prop=0.75,strata=High_Intensity_Billing)

df_train <- df_split %>% training()
df_test  <- df_split %>% testing()

# LOGISTIC REGRESSION MODEL
fitted_logistic_model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(High_Intensity_Billing~.,data=df_train)

tidy(fitted_logistic_model, exponentiate=TRUE)

pred_class <- predict(fitted_logistic_model,new_data=df_test,type="class")
pred_prob  <- predict(fitted_logistic_model,new_data=df_test,type="prob")

# RANDOM FOREST
# fitted_forest_model <- rand_forest(mode="classification") %>%
#   set_engine("ranger") %>%
#   fit(High_Intensity_Billing~.,data=df_train)
# 
# pred_class <- predict(fitted_forest_model,new_data=df_test,type="class")
# pred_prob  <- predict(fitted_forest_model,new_data=df_test,type="prob")

results <- df_test %>%
  select(High_Intensity_Billing) %>%
  bind_cols(pred_class,pred_prob)

conf_mat(results,truth=High_Intensity_Billing,estimate=.pred_class)
accuracy(results,truth=High_Intensity_Billing,estimate=.pred_class)
sens(results, truth = High_Intensity_Billing ,estimate =.pred_class)
spec(results, truth = High_Intensity_Billing ,estimate =.pred_class)
precision(results, truth = High_Intensity_Billing ,estimate =.pred_class)
