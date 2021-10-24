library(data.table)
library(tidytable)
library(dplyr)
library(tidyr)
library(tidymodels)
library(parallel)
library(tictoc)
library(ggplot2)

model_elements <- readRDS("data-analysis/model_elements.rds")

table <- data.frame(model_elements) %>% slice(3:223) %>%
  rename(Variable=model_elements)

sample_size <- 10000

for (i in 2006:2018) {
    year_ <- i-2000
    if (year_<10) {
      data = paste0("neds0",year_)
    }
    if (year_>=10) {
      data = paste0("neds",year_)
    }
    source <- paste0("data-analysis/",data,"-analysis-subsample.rds")
    data <- readRDS(source)
    
    df <- data %>% 
      filter.(EM_Code!="Not listed") %>%
      sample_n(sample_size) %>%
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
    
    # BUILD CROSSWALK  
    # crosswalk_ <- paste0(data,"-crosswalk.R")
    # source("crosswalk_")
    
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
    #  filter(Count>50) %>%
      select(value) %>%
      rename(Code=value)
    ICD <- ICD %>%
      filter(value %in% ICD_List$Code) %>%
      pivot_wider(names_from=value,names_prefix="ICD_",values_from=Constant) 
    ICD <- ICD %>%
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
      unique() %>%
      pivot_wider(names_from=value,names_prefix="CPT",values_from=Constant) %>%
      mutate_all(~ifelse(is.na(.),0,.))
    CPT <- 
      cbind(
        CPT$rowname,
        select_if(CPT,is.numeric)#[,colSums(select_if(CPT,is.numeric))>1000]
      )
    colnames(CPT)[1] <- "rowname"
    
    df <- df %>%  
      select(High_Intensity_Billing,AGE) %>%
      mutate(rowname=paste0("Row",rep(1:n()))) %>%
      left_join(ICD,by="rowname") %>%
      left_join(CPT,by="rowname")
    df <- df %>%  select(-rowname)
    df[is.na(df)] <- 0
    rm(ICD,CPT)
    
    df <- df %>% mutate.(High_Intensity_Billing=factor(High_Intensity_Billing))
    df$High_Intensity_Billing <- relevel(df$High_Intensity_Billing,ref="99281-99284")
    
    input <- data.frame(matrix(ncol = length(model_elements), nrow = 0))
    colnames(input) <- model_elements
    input$High_Intensity_Billing <- factor(input$High_Intensity_Billing)
    
    input <- bind_rows(input,df) %>%
      select(all_of(model_elements))
    input[is.na(input)] <- 0
    rm(df)
    
    frequency_ <- data.frame(colSums(select(input,-High_Intensity_Billing,-AGE)))
    colnames(frequency_) <- paste0("Frequency_",year_)
    
    frequency_ <- tibble::rownames_to_column(frequency_, "Variable")
    
    table <- table %>%
      left_join(frequency_,by="Variable")
    print(paste0(i," completed."))
}