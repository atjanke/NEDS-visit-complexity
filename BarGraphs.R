library(data.table)
library(tidytable)
library(dplyr)
library(tidyr)
library(tidymodels)
library(parallel)
library(tictoc)

neds06 <- readRDS("data-analysis/neds06-analysis-subsample.rds") %>% mutate(Year=2006)
neds12 <- readRDS("data-analysis/neds12-analysis-subsample.rds") %>% mutate(Year=2012)
neds18 <- readRDS("data-analysis/neds18-analysis-subsample.rds") %>% mutate(Year=2018)


reshaped06 <- neds06 %>%
        mutate.(AGE = case_when(
          (AGE>= 18 & AGE<44) ~ 1,
          AGE>= 45 & AGE<65  ~ 2,
          AGE>= 65 & AGE<85  ~ 3,
          AGE>=85            ~ 4
        )) %>%
        mutate.(ELIX = case_when(
          ELIX>2 ~ 3,
          T ~ ELIX
        )) %>%
        mutate.(ELIX = factor(ELIX,levels=c("0","1","2","3"))) %>%
        summarize.(Proportion = n.()/nrow(neds06),.by=c("AGE","ELIX")) %>%
        mutate.(Year = 2006) %>% arrange(AGE,ELIX)

reshaped12 <- neds12 %>%
          mutate.(AGE = case_when(
            (AGE>= 18 & AGE<44) ~ 1,
            AGE>= 45 & AGE<65  ~ 2,
            AGE>= 65 & AGE<85  ~ 3,
            AGE>=85            ~ 4
          )) %>%
          mutate.(ELIX = case_when(
            ELIX>2 ~ 3,
            T ~ ELIX
          )) %>%
          mutate.(ELIX = factor(ELIX,levels=c("0","1","2","3"))) %>%
          summarize.(Proportion = n.()/nrow(neds12),.by=c("AGE","ELIX")) %>%
          mutate.(Year = 2012) %>% arrange(AGE,ELIX)

reshaped18 <- neds18 %>%
        mutate.(AGE = case_when(
          (AGE>= 18 & AGE<44) ~ 1,
           AGE>= 45 & AGE<65  ~ 2,
           AGE>= 65 & AGE<85  ~ 3,
           AGE>= 85           ~ 4
        )) %>%
        mutate.(ELIX = case_when(
          ELIX>2 ~ 3,
          T ~ ELIX
        )) %>%
        mutate.(ELIX = factor(ELIX,levels=c("0","1","2","3"))) %>%
        summarize.(Proportion = n.()/nrow(neds18),.by=c("AGE","ELIX")) %>%
        mutate.(Year = 2018) %>% arrange(AGE,ELIX)

table<-reshaped06 %>%
  rbind(reshaped12) %>%
  rbind(reshaped18) %>%
  mutate.(ELIX = case_when(
    ELIX=="0" ~ "Elix Score: 0",
    ELIX=="1" ~ "Elix Score: 1",
    ELIX=="2" ~ "Elix Score: 2",
    ELIX=="3" ~ "Elix Score: 3+",
  )) %>%
  mutate.(Year = factor(Year,levels=c("2006","2012","2018"))) %>%
  group_by(ELIX,Year) %>%
  summarise(Proportion=sum(Proportion))



p <- reshaped06 %>%
  rbind(reshaped12) %>%
  rbind(reshaped18) %>%
  mutate.(ELIX = case_when(
    ELIX=="0" ~ "Elix Score: 0",
    ELIX=="1" ~ "Elix Score: 1",
    ELIX=="2" ~ "Elix Score: 2",
    ELIX=="3" ~ "Elix Score: 3+",
  )) %>%
  mutate.(Year = factor(Year,levels=c("2006","2012","2018"))) %>%
  filter(ELIX!="Elix Score: 0") %>%
  ggplot(aes(x=AGE,y=Proportion,group=Year,fill=Year))+
  geom_bar(position="dodge",stat="identity")+
  scale_fill_manual(values=c(
    "2006"="#440154FF",
    "2012"="#21908CFF",
    "2018"="#FDE725FF"
  ))+
  scale_x_continuous(breaks=c(1,2,3,4),labels=c("18-44","45-64","65-84","85+"))+xlab("")+
  scale_y_continuous(labels=scales::percent_format(accuracy=0.1))+ylab("% of Treat-and-Release ED Visits")+
  facet_wrap(~ELIX,nrow=1)+
  theme_bw()
p
ggsave('Figures/BarGraph.tiff',p,height=5,width=7,dpi=600)

  