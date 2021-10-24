library(data.table)
library(tidytable)
library(dplyr)
library(tidyr)
library(tidymodels)
library(parallel)
library(tictoc)
library(stringr)

cores <- detectCores()-3

build_test <- function(build,test) {
  source <- paste0("data-analysis/",build,"-analysis-subsample.rds")
  data <- readRDS(source)
  
  df <- data %>% 
    filter.(EM_Code!="Not listed") %>%
    sample_n(100000) %>%
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
      DISP_ED,
      DXCCS1,ELIX,PRCCSED1:PRCCSED9)
  
  df <- df %>% mutate.(High_Intensity_Billing=factor(High_Intensity_Billing))
  df$High_Intensity_Billing <- relevel(df$High_Intensity_Billing,ref="99281-99284")
  
  # list_of_elements <-  df %>%
  #   select(DXCCS1:DXCCS15) %>%
  #   pivot_longer(DXCCS1:DXCCS15) %>%
  #   select(value) %>%
  #   unique() %>% 
  #   mutate(value = paste0(value)) %>%
  #   list()
  
  df <- df %>%
    mutate(DXCCS1 = factor(DXCCS1)) #%>%
    # mutate(DXCCS2 = factor(DXCCS2)) %>%
    # mutate(DXCCS3 = factor(DXCCS3)) %>%
    # mutate(DXCCS4 = factor(DXCCS4)) %>%
    # mutate(DXCCS5 = factor(DXCCS5)) %>%
    # mutate(DXCCS6 = factor(DXCCS6)) %>%
    # mutate(DXCCS7 = factor(DXCCS7)) %>%
    # mutate(DXCCS8 = factor(DXCCS8)) %>%
    # mutate(DXCCS9 = factor(DXCCS9)) %>%
    # mutate(DXCCS10 = factor(DXCCS10)) %>%
    # mutate(DXCCS11 = factor(DXCCS11)) %>%
    # mutate(DXCCS12 = factor(DXCCS12)) %>%
    # mutate(DXCCS13 = factor(DXCCS13)) %>%
    # mutate(DXCCS14 = factor(DXCCS14)) %>%
    # mutate(DXCCS15 = factor(DXCCS15))
  

  print("The rate of high-intensity billing in the modeled year: ")
  df %>% mutate(Indicator=ifelse(High_Intensity_Billing!="99281-99284",1,0)) %>%
    summarise(Proportion=sum(Indicator)/n()) %>% print()
  
  df_split <- initial_split(df,prop=0.75,strata=High_Intensity_Billing)
  
  df_train <- df_split %>% training()
  df_test  <- df_split %>% testing()
  
  
  
  for(attr in colnames(df_train))
  {
    if (is.factor(df_train[[attr]]))
    {
      new.levels <- setdiff(levels(df_train[[attr]]), levels(df_test[[attr]]))
      if ( length(new.levels) == 0 )
      { print(paste(attr, '- no new levels')) }
      else
      {
        print(c(paste(attr, length(new.levels), 'of new levels, e.g.'), head(new.levels, 2)))
        levels(df_test[[attr]]) <- union(levels(df_test[[attr]]), levels(df_train[[attr]]))
      }
    }
  }
  
  rf_model <- rand_forest(mode="classification") %>%
    set_engine("ranger",num.threads=cores) %>%
    fit(High_Intensity_Billing~.,data=df_train)
  
  
  rf_results <- df_test %>%
    filter(is.na(DXCCS1)==FALSE) %>%
    select(High_Intensity_Billing) %>%
    bind_cols(predict(rf_model,new_data=(df_test %>% filter(is.na(DXCCS1)==FALSE)),type="class"),
              predict(rf_model,new_data=(df_test %>% filter(is.na(DXCCS1)==FALSE)),type="prob"))
  colnames(rf_results) <- c("High_Intensity_Billing",".pred_class","pred_High_Intes","pred9928")
  
  print("Predicted number of high-intensity visits in the hold out set: ")
  a<-colSums(rf_results[,4])
  print(a)
  print("Observed number of high-intensity visits in the hold out set: ") 
  b<-nrow(df_test[High_Intensity_Billing=="High-Intensity"])
  print(b)
  print("Percent difference: ")
  print(1- a/b)
  
  rf_conf_mat <- conf_mat (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)
  
  performance_table <- data.frame()
  performance_table[1,1] <- accuracy (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
  performance_table[1,2] <- sens     (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
  performance_table[1,3] <- spec     (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
  performance_table[1,4] <- precision(rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
  performance_table[1,5] <- recall   (rf_results, truth = High_Intensity_Billing,estimate = .pred_class)[3]
  performance_table[1,6] <- roc_auc  (rf_results, truth = High_Intensity_Billing,estimate = pred_High_Intes)[3]
  
  
  colnames(performance_table) <- c("accuracy","sens","spec","precision","recall","auroc")
  
  
  
  
  
  source <- paste0("data-analysis/",test,"-analysis-subsample.rds")
  data <- readRDS(source)
  
  df_test <- data %>%
    filter.(EM_Code!="Not listed") %>%
    sample_n(250000) %>%
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
      DISP_ED,
      DXCCS1,ELIX,PRCCSED1:PRCCSED9)
  
  df_test <- df_test %>%
    mutate(DXCCS1=factor(DXCCS1)) %>%
    filter(is.na(DXCCS1)==FALSE)
  
  df_test <- df_test %>%
    mutate.(DXCCS1 = factor(DXCCS1,levels=levels(df_train$DXCCS1))) %>%
    filter.(is.na(DXCCS1)==FALSE)
  
  pred_class <- predict(rf_model,new_data=df_test,type="class")
  pred_prob  <- predict(rf_model,new_data=df_test,type="prob")
    
  results <- df_test %>%
    select(High_Intensity_Billing) %>%
    bind_cols(pred_class,pred_prob)
  
  estimated <- floor(sum(results[,4]))
  observed <- nrow(results[results$High_Intensity_Billing=="High-Intensity",])
  proportion_obs <- round(observed/nrow(results),4)
  proportion_est <- round(estimated/nrow(results),4)
  
  print(paste0("Estimated high-intensity billing count: ",estimated))
  print(paste0("Observed high-intensity billing count: ", observed))
  print(paste0("Rate: ",proportion_est," | expected high-intensity billing"))
  print(paste0("Rate: ",proportion_obs," | observed high-intensity billing"))
  gc()
}

