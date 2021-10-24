library(data.table)
library(tidytable)
library(dplyr)
library(tidyr)
library(tidymodels)
library(parallel)
library(tictoc)
library(ggplot2)

# First, load the models into memory
logistic06 <- readRDS("data-analysis/logistic06.rds")
forest06   <- readRDS("data-analysis/forest06.rds")

model_elements <- data.frame(readRDS("data-analysis/model_elements.rds"))
colnames(model_elements) <- c("Variables")

source("Test-Model.R")

table <- data.frame()

# Choose sample size for the draws on subsequent years, loop through
sample_ <- 20000
for (i in 2007:2018) {
  print("..................")
  print(paste0("Implementing: ",i))
  print("......")
  print("......")
  index_ = i-2006
  year_ = i-2000
  if (year_<10) {
    data_ = paste0("neds0",year_)
  }
  if (year_>=10) {
    data_ = paste0("neds",year_)
  }
  print(paste0("Testing ",data_))
  table[index_,1] <- i
  table[index_,2] <- test_model(data_,sample_,logistic06)
  table[index_,3] <- test_model(data_,sample_,forest06)
  gc()
}
colnames(table) <- c("Year","Logistic_OE_Ratio","RF_OE_Ratio")

write.csv(table,"Results/Observed_Predicted_Ratios.csv")



# Tack on observed as percent of ratio in 2006
for (i in 2007:2018){
  index_=i-2006
  year_ =i-2000
  if (year_<10) {data_ = paste0("neds0",year_)}
  if (year_>=10) {data_ = paste0("neds",year_)}
  source_ = paste0("data-analysis/",data_,"-analysis-subsample.rds")
  neds <- readRDS(source_) %>% mutate.(High_Intensity_Billing=case_when(EM_Code=="99285" | EM_Code=="99291"~1,T~0))
  table[index_,4] = (sum(neds$High_Intensity_Billing==1)/nrow(neds))
}
  

# Plot
table<-table %>%
  rbind(c(2006,0.05137,0.05137,0.05317)) %>%
  pivot_longer(Logistic_OE_Ratio:V4) %>%
  mutate(name=case_when(
    name=="Logistic_OE_Ratio" ~ "Logistic",
    name=="RF_OE_Ratio" ~ "Random Forest",
    name=="V4" ~ "Observed")) %>%
  rename(Model=name) %>%
  mutate(Model = factor(Model,levels=c(
    "Observed","Random Forest","Logistic")))

p<-ggplot(table,aes(x=Year,y=value,group=Model,color=Model))+
  geom_line()+xlab("")+ylab("Percent of Visits High-Intensity")+
  scale_y_continuous(limits=c(0,.2),labels=scales::percent_format())+
  theme_bw()+
  geom_hline(yintercept=0.051,linetype="dashed",alpha=0.4)
p
ggsave("Results/OE_Ratio.tiff",dpi=300)

