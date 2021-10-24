library(data.table)
library(tidytable)
library(dplyr)
library(tidyr)
library(tidymodels)
library(parallel)
library(tictoc)
library(ggplot2)

#############
### ICD List
table <- data.frame()
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
  
  if (year_ <2015) {
  CCS_List <- data %>%
    select.(DXCCS1:DXCCS15) %>%
    mutate.(rowname=paste0("Row",rep(1:n.()))) %>%
    pivot_longer.(cols=-rowname) %>%
    filter.(value!=-999) %>%
    summarise.(Count=n.(),Proportion=n.()/nrow(data),.by="value") %>%
    mutate.(value = as.character(value))
  }
  
  if (year_ >=2015) {
    CCS_List <- data %>%
      select.(DXCCS1:DXCCS15) %>%
      mutate.(rowname=paste0("Row",rep(1:n.()))) %>%
      pivot_longer.(cols=-rowname) %>%
      filter.(value!="-999") %>%
      summarise.(Count=n.(),Proportion=n.()/nrow(data),.by="value")
  }
  
  setnames(CCS_List,c("value","Count","Proportion"), c("CCS",paste0("Count_",year_),paste0("Proportion_",year_)))
  if (year_==6) {table <- CCS_List}
  if (year_>6 ) {
    table <- table %>%
      merge(CCS_List,by="CCS",all=TRUE)
  }
}

write.csv(table,"descriptive-output/CCS-Counts-Proportions.csv")

# Load CCS labels
ccs <- fread("ccs.csv") %>% select(c(1:4))
colnames(ccs) <- c("ICD9","CCS","CCS_Description","ICD9_Description")

ccs <-  ccs %>%
  mutate.(ICD9 = str_replace_all(ICD9, "[[:punct:]]", ""),
          CCS  = as.numeric(str_replace_all(CCS, "[[:punct:]]", "")),
          CCS_Description = 
            str_replace_all(CCS_Description,"[[:punct:]]", "")) 




















#############
### CPT List
table <- data.frame()
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

  CPT_List <- data %>%
              select.(CPT1:CPT15) %>%
              mutate.(rowname=paste0("Row",rep(1:n.()))) %>%
              pivot_longer.(cols=-rowname) %>%
              filter.(value!="" & value!="invl") %>%
              summarise.(Count=n.(),Proportion=n.()/nrow(data),.by="value") %>%
              filter.(value!="99281" &
                      value!="99282" &
                      value!="99283" &
                      value!="99284" &
                      value!="99285" &
                      value!="99291")
  
  setnames(CPT_List,c("value","Count","Proportion"), c("CPT",paste0("Count_",year_),paste0("Proportion_",year_)))
  if (year_==6) {table <- CPT_List}
  if (year_>6 ) {
  table <- table %>%
    merge(CPT_List,by="CPT",all=TRUE)
  }
}

write.csv(table,"descriptive-output/CPT-Counts-Proportions.csv")







##### Change in CPT code counts
df <- table %>%
  mutate.(Change = Proportion_18/Proportion_6) %>%
  arrange.(-Change) %>%
  filter(Count_6>100) %>%
  select.(CPT,Proportion_6,Proportion_12,Proportion_18,Change)


