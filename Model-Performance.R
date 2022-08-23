#### Libraries                                  ####
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
#### Performance Summary                        ####

table <- rbind(
  readRDS("02_Results/Logistic-2006-Performance.rds"),
  readRDS("02_Results/Logistic-2007-Performance.rds"),
  readRDS("02_Results/Logistic-2008-Performance.rds"),
  readRDS("02_Results/Logistic-2009-Performance.rds"),
  readRDS("02_Results/Logistic-2010-Performance.rds"),
  readRDS("02_Results/Logistic-2011-Performance.rds"),
  readRDS("02_Results/Logistic-2012-Performance.rds"),
  readRDS("02_Results/Logistic-2013-Performance.rds"),
  readRDS("02_Results/Logistic-2014-Performance.rds"),
  readRDS("02_Results/Logistic-2015-Performance.rds"),
  readRDS("02_Results/Logistic-2016-Performance.rds"),
  readRDS("02_Results/Logistic-2017-Performance.rds"),
  readRDS("02_Results/Logistic-2018-Performance.rds")) %>%
  mutate(OE = observed.subseq.year/pred.subseq.year) %>%
  mutate(O  = observed.subseq.year/lag(observed.subseq.year,1)) %>%
  mutate(Unaccounted.Trend = cumprod(OE)) %>%
  mutate(Observed.Trend = observed.subseq.year /0.049)



#### Supplemental Table: Model Building         ####

table %>%
  mutate.(
    outcome.param = (obs.hold.out/n.test)*n.train/n.parameters,
    obs.HIB = (obs.hold.out/n.test),
    esti.HIB = (pred.hold.out/n.test)
  ) %>%
  select.(
    n.parameters,n.train,outcome.param,n.test,obs.HIB,esti.HIB,sens,spec,auroc) %>%
  write.csv("03_Tables/Supplemental-Table-Model-Building.csv")

#### Supplemental Table: Observed and Estimated ####
table %>%
  select.(observed.subseq.year,pred.subseq.year,OE,Unaccounted.Trend) %>%
  write.csv("03_Tables/Supplemental-Table-Observed-and-Estimated.csv")