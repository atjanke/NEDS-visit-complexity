#### Libraries                   ####
library(tidyverse)
library(data.table)
library(tidytable)
library(tidymodels)
library(tictoc)
library(parallel)
library(gridExtra)
library(forcats)

#### Figure 1 (data)             ####
tic(msg="Load Data for Figure 1")
neds19 <- readRDS("__data-analysis/neds19-analysis.rds")
neds18 <- readRDS("__data-analysis/neds18-analysis.rds")
neds17 <- readRDS("__data-analysis/neds17-analysis.rds")
neds16 <- readRDS("__data-analysis/neds16-analysis.rds")
neds15 <- readRDS("__data-analysis/neds15-analysis.rds")
neds14 <- readRDS("__data-analysis/neds14-analysis.rds")
neds13 <- readRDS("__data-analysis/neds13-analysis.rds")
neds12 <- readRDS("__data-analysis/neds12-analysis.rds")
neds11 <- readRDS("__data-analysis/neds11-analysis.rds")
neds10 <- readRDS("__data-analysis/neds10-analysis.rds")
neds09 <- readRDS("__data-analysis/neds09-analysis.rds")
neds08 <- readRDS("__data-analysis/neds08-analysis.rds")
neds07 <- readRDS("__data-analysis/neds07-analysis.rds")
neds06 <- readRDS("__data-analysis/neds06-analysis.rds")

trend <-    rbind(
  neds19 %>% summarise.(Proportion = n.()/nrow(neds19),.by="EM_Code") %>% mutate.(Year=2019),
  neds18 %>% summarise.(Proportion = n.()/nrow(neds18),.by="EM_Code") %>% mutate.(Year=2018),
  neds17 %>% summarise.(Proportion = n.()/nrow(neds17),.by="EM_Code") %>% mutate.(Year=2017),
  neds16 %>% summarise.(Proportion = n.()/nrow(neds16),.by="EM_Code") %>% mutate.(Year=2016),
  neds15 %>% summarise.(Proportion = n.()/nrow(neds15),.by="EM_Code") %>% mutate.(Year=2015),
  neds14 %>% summarise.(Proportion = n.()/nrow(neds14),.by="EM_Code") %>% mutate.(Year=2014),
  neds13 %>% summarise.(Proportion = n.()/nrow(neds13),.by="EM_Code") %>% mutate.(Year=2013),
  neds12 %>% summarise.(Proportion = n.()/nrow(neds12),.by="EM_Code") %>% mutate.(Year=2012),
  neds11 %>% summarise.(Proportion = n.()/nrow(neds11),.by="EM_Code") %>% mutate.(Year=2011),
  neds10 %>% summarise.(Proportion = n.()/nrow(neds10),.by="EM_Code") %>% mutate.(Year=2010),
  neds09 %>% summarise.(Proportion = n.()/nrow(neds09),.by="EM_Code") %>% mutate.(Year=2009),
  neds08 %>% summarise.(Proportion = n.()/nrow(neds08),.by="EM_Code") %>% mutate.(Year=2008),
  neds07 %>% summarise.(Proportion = n.()/nrow(neds07),.by="EM_Code") %>% mutate.(Year=2007),
  neds06 %>% summarise.(Proportion = n.()/nrow(neds06),.by="EM_Code") %>% mutate.(Year=2006))

trend <- trend %>%
  mutate.(Level = case_when(
    EM_Code == "99281" ~ "99281",
    EM_Code == "99282" ~ "99282",
    EM_Code == "99283" ~ "99283",
    EM_Code == "99284" ~ "99284",
    EM_Code == "99285" | EM_Code == "99291" ~ "High-Intensity Billing",
    EM_Code == "Not listed" ~ "Not listed")) %>%
  summarise.(Proportion=sum(Proportion),.by=c("Level","Year")) %>%
  rename(EM_Code=Level)

saveRDS(trend,"02_Results/Figure-1-Data.rds")
rm(list = ls())
gc()
toc()

#### Figure 1 (render)           ####
tic(msg="Render Figure 1")
trend <- readRDS("02_Results/Figure-1-Data.rds")

SCALEcolor = c("99281"="#440154FF",
               "99282"="#3B528BFF",
               "99283"="#21908CFF",
               "99284"="#5DC863FF",
               "High-Intensity Billing"="#FDE725FF")
p <- trend %>%
  filter.(EM_Code!="Not listed") %>%
  mutate.(EM_Code = factor(EM_Code,levels=c(
    "99281","99282","99283","99284","High-Intensity Billing"))) %>%
  ggplot(aes(x=Year,y=Proportion,group=EM_Code,color=EM_Code))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=SCALEcolor)+
  guides(color=guide_legend(title="E/M Code"))+
  scale_y_continuous(limits=c(0,0.4),breaks=c(0.1,0.2,0.3,0.4),labels=scales::percent_format())+
  ylab("Percent of Treat-and-Release ED Visits")+
  scale_x_continuous(breaks=c(2007,2010,2013,2016,2019))+xlab("")+
  theme_bw()
p
ggsave('04_Figures/Figure-1.pdf',p,height=9/2,width=16/2,dpi=600)
rm(list = ls())
gc()
toc()



#### Figure 2 (data,render)      ####
neds19 <- readRDS("__data-analysis/neds19-analysis.rds")
neds18 <- readRDS("__data-analysis/neds18-analysis.rds")
neds17 <- readRDS("__data-analysis/neds17-analysis.rds")
neds16 <- readRDS("__data-analysis/neds16-analysis.rds")
neds15 <- readRDS("__data-analysis/neds15-analysis.rds")
neds14 <- readRDS("__data-analysis/neds14-analysis.rds")
neds13 <- readRDS("__data-analysis/neds13-analysis.rds")
neds12 <- readRDS("__data-analysis/neds12-analysis.rds")
neds11 <- readRDS("__data-analysis/neds11-analysis.rds")
neds10 <- readRDS("__data-analysis/neds10-analysis.rds")
neds09 <- readRDS("__data-analysis/neds09-analysis.rds")
neds08 <- readRDS("__data-analysis/neds08-analysis.rds")
neds07 <- readRDS("__data-analysis/neds07-analysis.rds")
neds06 <- readRDS("__data-analysis/neds06-analysis.rds")

a<-rbind(neds06 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds06),.by="ELIX_CAT") %>%mutate.(Year=2006),
         neds07 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds07),.by="ELIX_CAT") %>%mutate.(Year=2007),
         neds08 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds08),.by="ELIX_CAT") %>%mutate.(Year=2008),
         neds09 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds09),.by="ELIX_CAT") %>%mutate.(Year=2009),
         neds10 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds10),.by="ELIX_CAT") %>%mutate.(Year=2010),
         neds11 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds11),.by="ELIX_CAT") %>%mutate.(Year=2011),
         neds12 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds12),.by="ELIX_CAT") %>%mutate.(Year=2012),
         neds13 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds13),.by="ELIX_CAT") %>%mutate.(Year=2013),
         neds14 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds14),.by="ELIX_CAT") %>%mutate.(Year=2014),
         neds15 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds15),.by="ELIX_CAT") %>%mutate.(Year=2015),
         neds16 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds16),.by="ELIX_CAT") %>%mutate.(Year=2016),
         neds17 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds17),.by="ELIX_CAT") %>%mutate.(Year=2017),
         neds18 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds18),.by="ELIX_CAT") %>%mutate.(Year=2018),
         neds19 %>%mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>%summarise.(Proportion = n.()/nrow(neds19),.by="ELIX_CAT") %>%mutate.(Year=2019)) %>%
  mutate.(ELIX_CAT = case_when(
    ELIX_CAT=="(-1,0]"~"0",ELIX_CAT=="(0,1]"~"1",ELIX_CAT=="(1,1e+03]"~"2+",)) %>%
  mutate.(ELIX_CAT = factor(ELIX_CAT,levels=c("2+","1","0")))
a<-ggplot(a,aes(x=Year,y=Proportion,fill=ELIX_CAT))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c("2+"="#FDE725FF",
                             "1"="#21908CFF",
                             "0"="#440154FF"))+
  labs(fill="# of Elixhauser\nComorbidities")+
  scale_y_continuous(labels=scales::percent_format(accuray=1))+
  scale_x_continuous(breaks=c(2006,2010,2014,2018))+
  xlab("")+ylab("Percent of Treat-and-Release ED Visits")+
  theme_bw()+
  ggtitle("Panel A")

b <-    rbind(
  neds19 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2019),
  neds18 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2018),
  neds17 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2017),
  neds16 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2016),
  neds15 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2015),
  neds14 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2014),
  neds13 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2013),
  neds12 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2012),
  neds11 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2011),
  neds10 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2010),
  neds09 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2009),
  neds08 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2008),
  neds07 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2007),
  neds06 %>% mutate.(ELIX_CAT = cut(ELIX,c(-1,0,1,1000))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="ELIX_CAT") %>% mutate.(Year=2006))

b <- b %>%
  mutate.(ELIX_CAT = case_when(
    ELIX_CAT=="(-1,0]"~"0",ELIX_CAT=="(0,1]"~"1",ELIX_CAT=="(1,1e+03]"~"2+",)) %>%
  mutate.(ELIX_CAT = factor(ELIX_CAT,levels=c("2+","1","0")))
saveRDS(b,"02_Results/Figure-2b-Data.rds")
b <- ggplot(b, aes(x=Year,y=Proportion,group=ELIX_CAT,color=ELIX_CAT))+geom_line()+geom_point()+
  scale_color_manual(values=c("2+"="#FDE725FF",
                              "1"="#21908CFF",
                              "0"="#440154FF"))+
  labs(color="# of Elixhauser\nComorbidities")+
  scale_y_continuous(labels=scales::percent_format(accuray=1),limits=c(0,0.45))+
  scale_x_continuous(breaks=c(2006,2010,2014,2018))+
  xlab("")+ylab("High-Intensity Billing Rate")+
  theme_bw()

c<-rbind(neds06 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds06),.by="AGE_CAT") %>%mutate.(Year=2006),
         neds07 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds07),.by="AGE_CAT") %>%mutate.(Year=2007),
         neds08 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds08),.by="AGE_CAT") %>%mutate.(Year=2008),
         neds09 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds09),.by="AGE_CAT") %>%mutate.(Year=2009),
         neds10 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds10),.by="AGE_CAT") %>%mutate.(Year=2010),
         neds11 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds11),.by="AGE_CAT") %>%mutate.(Year=2011),
         neds12 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds12),.by="AGE_CAT") %>%mutate.(Year=2012),
         neds13 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds13),.by="AGE_CAT") %>%mutate.(Year=2013),
         neds14 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds14),.by="AGE_CAT") %>%mutate.(Year=2014),
         neds15 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds15),.by="AGE_CAT") %>%mutate.(Year=2015),
         neds16 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds16),.by="AGE_CAT") %>%mutate.(Year=2016),
         neds17 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds17),.by="AGE_CAT") %>%mutate.(Year=2017),
         neds18 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds18),.by="AGE_CAT") %>%mutate.(Year=2018),
         neds19 %>%mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>%summarise.(Proportion = n.()/nrow(neds19),.by="AGE_CAT") %>%mutate.(Year=2019)) %>%
  mutate.(AGE_CAT = case_when(
    AGE_CAT=="(17,45]"~"18-44",
    AGE_CAT=="(45,65]"~"45-64",
    AGE_CAT=="(65,85]"~"65-84",
    AGE_CAT=="(85,125]"~"85+")) %>%
  mutate.(AGE_CAT = factor(AGE_CAT,levels=c("85+","65-84","45-64","18-44")))

c<-ggplot(c,aes(x=Year,y=Proportion,fill=AGE_CAT))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c(
    "85+"="#FAEBDDFF",
    "65-84"="#F69C73FF",
    "45-64"="#A11A5BFF",
    "18-44"="#03051AFF"))+
  labs(fill="Age\nCategory")+
  scale_y_continuous(labels=scales::percent_format(accuray=1))+
  scale_x_continuous(breaks=c(2006,2010,2014,2018))+
  xlab("")+#ylab("Percent of Treat-and-Release ED Visits")+
  theme_bw()+theme(axis.title.y=element_blank())+
  ggtitle("Panel B")

d <-    rbind(
  neds19 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2019),
  neds18 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2018),
  neds17 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2017),
  neds16 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2016),
  neds15 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2015),
  neds14 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2014),
  neds13 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2013),
  neds12 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2012),
  neds11 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2011),
  neds10 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2010),
  neds09 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2009),
  neds08 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2008),
  neds07 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2007),
  neds06 %>% mutate.(AGE_CAT=cut(AGE,c(17,45,65,85,125))) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="AGE_CAT") %>% mutate.(Year=2006))

d <- d %>%
  mutate.(AGE_CAT = case_when(
    AGE_CAT=="(17,45]"~"18-44",
    AGE_CAT=="(45,65]"~"45-64",
    AGE_CAT=="(65,85]"~"65-84",
    AGE_CAT=="(85,125]"~"85+")) %>%
  mutate.(AGE_CAT = factor(AGE_CAT,levels=c("85+","65-84","45-64","18-44")))
d <- ggplot(d, aes(x=Year,y=Proportion,group=AGE_CAT,color=AGE_CAT))+geom_line()+geom_point()+
  scale_color_manual(values=c(
    "85+"="#FAEBDDFF",
    "65-84"="#F69C73FF",
    "45-64"="#A11A5BFF",
    "18-44"="#03051AFF"))+
  labs(color="Age\nCategory")+
  scale_y_continuous(labels=scales::percent_format(accuray=1),limits=c(0,0.45))+
  scale_x_continuous(breaks=c(2006,2010,2014,2018))+
  xlab("")+
  theme_bw()+theme(axis.title.y=element_blank())

e<-rbind(neds06 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds06),.by="PAY1") %>%mutate.(Year=2006),
         neds07 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds07),.by="PAY1") %>%mutate.(Year=2007),
         neds08 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds08),.by="PAY1") %>%mutate.(Year=2008),
         neds09 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds09),.by="PAY1") %>%mutate.(Year=2009),
         neds10 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds10),.by="PAY1") %>%mutate.(Year=2010),
         neds11 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds11),.by="PAY1") %>%mutate.(Year=2011),
         neds12 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds12),.by="PAY1") %>%mutate.(Year=2012),
         neds13 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds13),.by="PAY1") %>%mutate.(Year=2013),
         neds14 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds14),.by="PAY1") %>%mutate.(Year=2014),
         neds15 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds15),.by="PAY1") %>%mutate.(Year=2015),
         neds16 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds16),.by="PAY1") %>%mutate.(Year=2016),
         neds17 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds17),.by="PAY1") %>%mutate.(Year=2017),
         neds18 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds18),.by="PAY1") %>%mutate.(Year=2018),
         neds19 %>%mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>%summarise.(Proportion = n.()/nrow(neds19),.by="PAY1") %>%mutate.(Year=2019)) %>%
  mutate.(PAY1=factor(PAY1,levels=c("Other","Self-Pay","Private Insurance","Medicare","Medicaid")))

e<-ggplot(e,aes(x=Year,y=Proportion,fill=fct_rev(PAY1)))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c(
                             "Medicaid"="#00204DFF",
                             "Medicare"="#414D6BFF",
                             "Private Insurance"="#7C7B78FF",
                             "Self-Pay"="#BCAF6FFF",
                             "Other"="#FFEA46FF"
                             ))+
  labs(fill="Insurance\nCategory")+
  scale_y_continuous(labels=scales::percent_format(accuray=1))+
  scale_x_continuous(breaks=c(2006,2010,2014,2018))+
  xlab("")+#ylab("Percent of Treat-and-Release ED Visits")+
  theme_bw()+theme(axis.title.y=element_blank())+
  ggtitle("Panel C")

f <-    rbind(
  neds19 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2019),
  neds18 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2018),
  neds17 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2017),
  neds16 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2016),
  neds15 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2015),
  neds14 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2014),
  neds13 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2013),
  neds12 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2012),
  neds11 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2011),
  neds10 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2010),
  neds09 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2009),
  neds08 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2008),
  neds07 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2007),
  neds06 %>% mutate.(PAY1=case_when(PAY1==1~"Medicare",PAY1==2~"Medicaid",PAY1==3~"Private Insurance",PAY1==4~"Self-Pay",T~"Other")) %>% mutate.(High_Intens = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>% summarise.(Proportion = sum(High_Intens)/n.(),.by="PAY1") %>% mutate.(Year=2006))

f <- ggplot(f, aes(x=Year,y=Proportion,group=PAY1,color=PAY1))+geom_line()+geom_point()+
  scale_color_manual(values=c(
    "Medicaid"="#00204DFF",
    "Medicare"="#414D6BFF",
    "Private Insurance"="#7C7B78FF",
    "Self-Pay"="#BCAF6FFF",
    "Other"="#FFEA46FF"
  ))+
  labs(color="Insurance\nCategory")+
  scale_y_continuous(labels=scales::percent_format(accuray=1),limits=c(0,0.45))+
  scale_x_continuous(breaks=c(2006,2010,2014,2018))+
  xlab("")+#ylab("High-Intensity Billing Rate")+
  theme_bw()+theme(axis.title.y=element_blank())

grid.arrange(a,c,e,b,d,f,nrow=2)
k <- 0.79
plot<-arrangeGrob(a,c,e,b,d,f,nrow=2,
                  widths=unit(c(5*k,4.65*k,5.1*k),c("in","in","in")))
ggsave("04_Figures/Figure-2.pdf",plot,width=15*k,height=8*k,dpi=600)




#### Figure 3 (data,render)      ####
a <- rbind(
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
  mutate(Observed.Trend = observed.subseq.year /0.049) %>%
  select.(Year,Unaccounted.Trend,Observed.Trend) %>%
  mutate.(
    Unaccounted.Trend=Unaccounted.Trend*0.049,
    Observed.Trend   =Observed.Trend*0.049) %>%
  mutate.(
    Unaccounted.Trend  =Observed.Trend - Unaccounted.Trend + 0.049) %>%
  pivot_longer(cols=Unaccounted.Trend:Observed.Trend) %>%
  rename(Trend=name) %>%
  mutate(Trend=case_when(
    Trend=="Unaccounted.Trend"~"Expected",
    T ~ "Observed"
  )) %>% mutate(Trend=factor(Trend,levels=c("Expected","Observed")))
a <- a %>%
  rbind(cbind(
    Year=2005,
    Trend="Expected",
    value=0.049
  )) %>%
  rbind(cbind(
    Year=2005,
    Trend="Observed",
    value=0.049
  )) %>%
  mutate(Year=as.integer(Year)) %>%
  mutate(value=as.numeric(value))



wide <- a %>% pivot_wider(id_cols=Year,names_from=Trend,values_from=value) %>%
  mutate(Trend="") %>%
  mutate(value=0)

p <- ggplot(a,aes(x=Year+1,y=value,group=Trend,color=Trend))+
  geom_line()+
  geom_point()+
  geom_ribbon(data=wide,
              aes(x=Year+1,ymax=Observed,ymin=Expected),fill="blue",alpha=0.20,colour = NA)+
  geom_ribbon(data=wide,
              aes(x=Year+1,ymax=Expected,ymin=0.048),fill="green",alpha=0.20,colour = NA)+
  scale_color_manual(values=c("Expected"="#440154FF","Observed"="#FDE725FF"))+
  scale_y_continuous(labels=scales::percent_format(accuracy=1),limits=c(0.047,0.20))+
  scale_x_continuous(breaks=c(2006,2008,2010,2012,2014,2016,2018))+
  # annotate(
  #   geom = "curve", x = 2018.5, y = 0.06, xend = 2018.5, yend = 0.105, 
  #   curvature = 0, arrow = arrow(length = unit(1.5, "mm")))+
  ylab("High-Intensity Billing Rate")+xlab("")+
  annotate("text", x = c(2017,2017), y = c(0.08,0.15), size=3,
           label = c("Expected\nIncrease","Unaccounted for\nby Case Mix"))+
  theme_bw()+
  theme(legend.title=element_blank())

ggsave('04_Figures/Figure-3.pdf',p,height=9/2,width=16/2,dpi=600)










####################################
####################################
####################################
####################################
####################################
#### Version 1 Figure 2, data    ####
reshaped06 <- neds06 %>%
  mutate.(AGE = case_when(
    (AGE>= 18 & AGE<44) ~ 1, AGE>= 45 & AGE<65  ~ 2,
    AGE>= 65 & AGE<85  ~ 3, AGE>=85            ~ 4)) %>%
  mutate.(ELIX = case_when(ELIX>1 ~ 2,T ~ ELIX)) %>%
  mutate.(ELIX = factor(ELIX,levels=c("0","1","2"))) %>%
  mutate.(High_Intensity = ifelse(EM_Code=="99285" | EM_Code=="99291",1,0)) %>%
  summarize.(Proportion = n.()/nrow(neds06),
             High_Intensity = sum(High_Intensity)/nrow(neds06),
             .by=c("AGE","ELIX")) %>%
  mutate.(Year = 2006) %>% arrange(AGE,ELIX)

reshaped13 <- neds13 %>%
  mutate.(AGE = case_when(
    (AGE>= 18 & AGE<44) ~ 1, AGE>= 45 & AGE<65  ~ 2, 
    AGE>= 65 & AGE<85  ~ 3, AGE>=85            ~ 4)) %>%
  mutate.(ELIX = case_when(
    ELIX>2 ~ 3,T ~ ELIX)) %>%
  mutate.(ELIX = factor(ELIX,levels=c("0","1","2","3"))) %>%
  mutate.(High_Intensity = ifelse(EM_Code=="99285" | EM_Code=="99291",1,0)) %>%
  summarize.(Proportion = n.()/nrow(neds13),
             High_Intensity = sum(High_Intensity)/nrow(neds13),
             .by=c("AGE","ELIX")) %>%
  mutate.(Year = 2013) %>% arrange(AGE,ELIX)

reshaped19 <- neds19 %>%
  mutate.(AGE = case_when(
    (AGE>= 18 & AGE<44) ~ 1,AGE>= 45 & AGE<65  ~ 2,
    AGE>= 65 & AGE<85  ~ 3,AGE>= 85           ~ 4)) %>%
  mutate.(ELIX = case_when(ELIX>2 ~ 3,T ~ ELIX)) %>%
  mutate.(ELIX = factor(ELIX,levels=c("0","1","2","3"))) %>%
  mutate.(High_Intensity = ifelse(EM_Code=="99285" | EM_Code=="99291",1,0)) %>%
  summarize.(Proportion = n.()/nrow(neds19),
             High_Intensity = sum(High_Intensity)/nrow(neds19),
             .by=c("AGE","ELIX")) %>%
  mutate.(Year = 2019) %>% arrange(AGE,ELIX)

table<-reshaped06 %>%
  rbind(reshaped13) %>%
  rbind(reshaped19) %>%
  mutate.(ELIX = case_when(
    ELIX=="0" ~ "Elix Score: 0",
    ELIX=="1" ~ "Elix Score: 1",
    ELIX=="2" ~ "Elix Score: 2",
    ELIX=="3" ~ "Elix Score: 3+",
  )) %>%
  mutate.(Year = factor(Year,levels=c("2006","2013","2019"))) %>%
  mutate.(AGE = case_when(
    is.na(AGE)~1,T~AGE
  )) %>%
  mutate.(AGE = case_when(
    AGE==2 ~ "45-64",
    AGE==3 ~ "65-84",
    AGE==4 ~ "85+", T~"18-44")) %>%
  summarize.(Proportion = sum(Proportion),
             High_Intensity = sum(High_Intensity),
             .by=c("AGE","ELIX","Year")) %>%
  arrange(AGE,ELIX,Year)

saveRDS(table,"02_Results/Figure-2-Data.rds")

rm(list = ls())
gc()
toc()

#### Version 1 Figure 2, render  ####
tic(msg="Render Figure 2")

table <- readRDS("02_Results/Figure-2-Data.rds")

table <- table %>%
  group_by(Year,ELIX) %>%
  summarise(
    High_Intensity = sum(High_Intensity),
    Low_Intensity = sum(Proportion)-sum(High_Intensity)) %>%
  pivot_longer(cols=High_Intensity:Low_Intensity) %>%
  rename(Proportion=value) %>%
  rename(Billing=name) %>%
  mutate(Billing = case_when(
    Billing=="High_Intensity" ~ "High-Intensity",T ~ "99281-99284")) %>%
  mutate(Billing = factor(Billing,levels=c("99281-99284","High-Intensity")))

p <- table %>%
  mutate.(Year = factor(Year,levels=c("2006","2013","2019"))) %>%
  ggplot(aes(x=Year,y=Proportion,group=Billing,fill=Billing))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c(
    "99281-99284"="#440154FF",
    "High-Intensity"="#FDE725FF"
  ))+
  xlab("")+
  scale_y_continuous(labels=scales::percent_format(accuracy=0.1))+ylab("% of Treat-and-Release ED Visits")+
  facet_wrap(~ELIX,nrow=1)+
  theme_bw()+
  theme(legend.position = c(0.62,0.8))
p
ggsave('04_Figures/Figure-2.tiff',height=8,width=7,dpi=600)
rm(list = ls())
gc()
toc()








#### Version 1 Figure 3, data    ####
# Load NEDS06 and link comorbidities
tic(msg="Load Data for Figure 3")

neds06 <- readRDS("__data-analysis/neds06-analysis-subsample.rds")
diag_list <- neds06 %>%
  select.(KEY_ED, DX1:DX15) %>%
  pivot_longer.(DX1:DX15) %>%
  rename(code=value) %>%
  select.(-name) %>%
  data.frame()

# https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf
library(comorbidity)
elix <- comorbidity(x=diag_list,id="KEY_ED",code="code",score="elixhauser",icd="icd9",assign0=FALSE) %>%
  select.(KEY_ED,chf:depre,score) %>%
  rename.(ELIX = score) %>%
  mutate.(ELIX = case_when(
    ELIX<0 ~ 0,
    T ~ ELIX))
rm(diag_list)
gc()

neds06 <- neds06 %>% left_join.(elix,by="KEY_ED")
rm(elix)
gc()


# Load NEDS13 and link comorbidities
neds13 <- readRDS("__data-analysis/neds13-analysis-subsample.rds")
diag_list <- neds13 %>%
  select.(KEY_ED, DX1:DX15) %>%
  pivot_longer.(DX1:DX15) %>%
  rename(code=value) %>%
  select.(-name) %>%
  data.frame()

# https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf
library(comorbidity)
elix <- comorbidity(x=diag_list,id="KEY_ED",code="code",score="elixhauser",icd="icd9",assign0=FALSE) %>%
  select.(KEY_ED,chf:depre,score) %>%
  rename.(ELIX = score) %>%
  mutate.(ELIX = case_when(
    ELIX<0 ~ 0,
    T ~ ELIX))
rm(diag_list)
gc()

neds13 <- neds13 %>% left_join.(elix,by="KEY_ED")
rm(elix)
gc()

# Load NEDS19 and link comorbidities
neds19 <- readRDS("__data-analysis/neds19-analysis-subsample.rds")
diag_list <- neds19 %>%
  select.(KEY_ED, DX1:DX15) %>%
  pivot_longer.(DX1:DX15) %>%
  rename(code=value) %>%
  select.(-name) %>%
  data.frame()

# https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf
library(comorbidity)
elix <- comorbidity(x=diag_list,id="KEY_ED",code="code",score="elixhauser",icd="icd10",assign0=TRUE) %>%
  select.(KEY_ED,chf:depre,score) %>%
  rename.(ELIX = score) %>%
  mutate.(ELIX = case_when(
    ELIX<0 ~ 0,
    T ~ ELIX))
rm(diag_list)
gc()

neds19 <- neds19 %>% left_join.(elix,by="KEY_ED")
rm(elix)




# Build DF
df<- rbind(
  neds06 %>%
    filter.(AGE>=18 & AGE<65) %>%
    mutate.(High_Intensity = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>%
    select.(High_Intensity,chf:depre) %>%
    pivot_longer.(cols=c(chf:depre)) %>%
    summarize.(Proportion=sum(value)/n.(),
               High_Intensity = sum(value*High_Intensity)/n.(),
               .by=c("name")) %>%
    mutate.(Age_Category="Age 18-64") %>%
    mutate.(Year="2006") %>%
    rename.(Comorbidity=name),
  
  neds06 %>%
    filter.(AGE>=65) %>%
    mutate.(High_Intensity = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>%
    select.(High_Intensity,chf:depre) %>%
    pivot_longer.(cols=c(chf:depre)) %>%
    summarize.(Proportion=sum(value)/n.(),
               High_Intensity = sum(value*High_Intensity)/n.(),
               .by=c("name")) %>%
    mutate.(Age_Category="Age 65+") %>%
    mutate.(Year="2006") %>%
    rename.(Comorbidity=name),
  
  neds13 %>%
    filter.(AGE>=18 & AGE<65) %>%
    mutate.(High_Intensity = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>%
    select.(High_Intensity,chf:depre) %>%
    pivot_longer.(cols=c(chf:depre)) %>%
    summarize.(Proportion=sum(value)/n.(),
               High_Intensity = sum(value*High_Intensity)/n.(),
               .by=c("name")) %>%
    mutate.(Age_Category="Age 18-64") %>%
    mutate.(Year="2013") %>%
    rename.(Comorbidity=name),
  
  neds13 %>%
    filter.(AGE>=65) %>%
    mutate.(High_Intensity = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>%
    select.(High_Intensity,chf:depre) %>%
    pivot_longer.(cols=c(chf:depre)) %>%
    summarize.(Proportion=sum(value)/n.(),
               High_Intensity = sum(value*High_Intensity)/n.(),
               .by=c("name")) %>%
    mutate.(Age_Category="Age 65+") %>%
    mutate.(Year="2013") %>%
    rename.(Comorbidity=name),
  
  neds19 %>%
    filter.(AGE>=18 & AGE<65) %>%
    mutate.(High_Intensity = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>%
    select.(High_Intensity,chf:depre) %>%
    pivot_longer.(cols=c(chf:depre)) %>%
    summarize.(Proportion=sum(value)/n.(),
               High_Intensity = sum(value*High_Intensity)/n.(),
               .by=c("name")) %>%
    mutate.(Age_Category="Age 18-64") %>%
    mutate.(Year="2019") %>%
    rename.(Comorbidity=name),
  
  neds19 %>%
    filter.(AGE>=65) %>%
    mutate.(High_Intensity = ifelse(EM_Code=="99285"|EM_Code=="99291",1,0)) %>%
    select.(High_Intensity,chf:depre) %>%
    pivot_longer.(cols=c(chf:depre)) %>%
    summarize.(Proportion=sum(value)/n.(),
               High_Intensity = sum(value*High_Intensity)/n.(),
               .by=c("name")) %>%
    mutate.(Age_Category="Age 65+") %>%
    mutate.(Year="2019") %>%
    rename.(Comorbidity=name))



df <- df %>%
  filter.(
    Comorbidity=="chf"   |
      Comorbidity=="cpd"   |
      Comorbidity=="hypc"  |
      Comorbidity=="diabc" |
      Comorbidity=="drug"  |
      Comorbidity=="rf") %>%
  mutate.(Comorbidity= case_when(
    Comorbidity=="chf"     ~ "Congestive Heart Failure",
    Comorbidity=="cpd"     ~ "COPD",
    Comorbidity=="hypc"    ~ "Hypertension, Complications",
    Comorbidity=="diabc"   ~ "Diabetes Mellitus, Complications",
    Comorbidity=="drug"    ~ "Substance Use Disorders",
    Comorbidity=="rf"      ~ "Renal Failure"))


saveRDS(df,"02_Results/Figure-3-Data.rds")
rm(list = ls())
gc()
toc()    
toc()
#### Version 1 Figure 3, render  ####
tic(msg="Render Figure 3")

df <- readRDS("02_Results/Figure-3-Data.rds")

df <- df %>%
  mutate(Low_Intensity=Proportion-High_Intensity) %>%
  pivot_longer(cols=c("Low_Intensity","High_Intensity")) %>%
  select(-Proportion) %>%
  rename(Billing=name,Proportion=value) %>%
  mutate(Billing = case_when(Billing=="Low_Intensity" ~ "99281-99284",T~"High-Intensity")) %>%
  mutate(Billing=factor(Billing,levels=c("99281-99284","High-Intensity"))) %>%
  group_by(Comorbidity,Year,Billing) %>%
  summarise(Proportion=sum(Proportion))
  

df %>%
  ggplot(aes(x=Year,y=Proportion,group=Billing,fill=Billing))+
  geom_bar(stat="identity",position="stack")+
  scale_fill_manual(values=c(
    "99281-99284"="#440154FF",
    "High-Intensity"="#FDE725FF"
  ))+
  scale_y_continuous(labels=scales::percent_format(accuracy=0.1))+ylab("% of Treat-and-Release ED Visits")+
  facet_wrap(~Comorbidity,scales="free",ncol=3)+
  xlab("")+
  theme_bw()+
  theme(legend.position = c(0.8,0.8))
ggsave('04_Figures/Figure-3.tiff',height=11,width=9,dpi=600)

rm(list = ls())
gc()
toc()
