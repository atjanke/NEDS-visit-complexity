library(dplyr)
library(tidytable)
library(tidyverse)
library(ggplot2)

# Build heatmap
# Read individual data
neds06 <- readRDS("data-analysis/neds06-analysis.rds") %>% mutate(Year=2006)
neds12 <- readRDS("data-analysis/neds12-analysis.rds") %>% mutate(Year=2012)
neds18 <- readRDS("data-analysis/neds18-analysis.rds") %>% mutate(Year=2018)

# Process
reshaped06 <- neds06 %>%
  mutate.(AGE = case_when(
    (AGE>= 18 & AGE<25) ~ 1,
    AGE>= 25 & AGE<30 ~ 2,
    AGE>= 30 & AGE<35 ~ 3,
    AGE>= 35 & AGE<40 ~ 4,
    AGE>= 40 & AGE<45 ~ 5,
    AGE>= 45 & AGE<50 ~ 6,
    AGE>= 50 & AGE<55 ~ 7,
    AGE>= 55 & AGE<60 ~ 8,
    AGE>= 60 & AGE<65 ~ 9,
    AGE>= 65 & AGE<70 ~ 10,
    AGE>= 70 & AGE<75 ~ 11,
    AGE>= 75 & AGE<80 ~ 12,
    AGE>= 80 & AGE<85 ~ 13,
    AGE>=85           ~ 14
  )) %>%
  mutate.(ELIX = case_when(
    ELIX>5 ~ 6,
    T ~ ELIX
  )) %>%
  summarize.(Proportion = n.()/nrow(neds06),.by=c("AGE","ELIX")) %>%
  mutate.(Year = 2006)

reshaped12 <- neds12 %>%
  mutate.(AGE = case_when(
    (AGE>= 18 & AGE<25) ~ 1,
    AGE>= 25 & AGE<30 ~ 2,
    AGE>= 30 & AGE<35 ~ 3,
    AGE>= 35 & AGE<40 ~ 4,
    AGE>= 40 & AGE<45 ~ 5,
    AGE>= 45 & AGE<50 ~ 6,
    AGE>= 50 & AGE<55 ~ 7,
    AGE>= 55 & AGE<60 ~ 8,
    AGE>= 60 & AGE<65 ~ 9,
    AGE>= 65 & AGE<70 ~ 10,
    AGE>= 70 & AGE<75 ~ 11,
    AGE>= 75 & AGE<80 ~ 12,
    AGE>= 80 & AGE<85 ~ 13,
    AGE>=85           ~ 14
  )) %>%
  mutate.(ELIX = case_when(
    ELIX>5 ~ 6,
    T ~ ELIX
  )) %>%
  summarize.(Proportion = n.()/nrow(neds12),.by=c("AGE","ELIX")) %>%
  mutate(Year = 2012)

reshaped18 <- neds18 %>%
  mutate.(AGE = case_when(
    (AGE>= 18 & AGE<25) ~ 1,
    AGE>= 25 & AGE<30 ~ 2,
    AGE>= 30 & AGE<35 ~ 3,
    AGE>= 35 & AGE<40 ~ 4,
    AGE>= 40 & AGE<45 ~ 5,
    AGE>= 45 & AGE<50 ~ 6,
    AGE>= 50 & AGE<55 ~ 7,
    AGE>= 55 & AGE<60 ~ 8,
    AGE>= 60 & AGE<65 ~ 9,
    AGE>= 65 & AGE<70 ~ 10,
    AGE>= 70 & AGE<75 ~ 11,
    AGE>= 75 & AGE<80 ~ 12,
    AGE>= 80 & AGE<85 ~ 13,
    AGE>=85           ~ 14
    )) %>%
  mutate.(ELIX = case_when(
    ELIX>5 ~ 6,
    T ~ ELIX
  )) %>%
  summarize.(Proportion = n.()/nrow(neds18),.by=c("AGE","ELIX")) %>%
  mutate(Year = 2018)

df <- reshaped06 %>%
  left_join(reshaped12,by=c("AGE","ELIX")) %>%
  rename(Prop_12 = Proportion.y) %>%
  left_join(reshaped18,by=c("AGE","ELIX")) %>%
  rename(Prop_18 = Proportion) %>%
  rename(Prop_06 = Proportion.x) %>%
  select.(-Year.x,-Year.y,-Year) %>%
  mutate(
    Change_12 = (Prop_12/Prop_06)-1,
    Change_18 = (Prop_18/Prop_06)-1) %>%
  select.(-Prop_18,-Prop_12,-Prop_06) %>%
  pivot_longer(Change_12:Change_18) %>%
  rename(Year=name,Change=value) %>%
  mutate(Year = case_when(
    Year=="Change_12" ~ "2012",
    Year=="Change_18" ~ "2018"
  ))
  

p <- df %>%
  filter.(AGE<85) %>%
  mutate.(Change = case_when(
    Change<0                 ~ "-25% to 0%",
    Change>=0   & Change<1   ~ "+0% to 100%",
    Change>=1   & Change<2   ~ "+100% to 200%",
    Change>=2   & Change<3   ~ "+200% to 300%",
    Change>=3   & Change<4   ~ "+300% to 400%",
    Change>=4                ~ ">400%")) %>%
  ggplot(aes(x=AGE,y=ELIX,
             fill=Change
             ))+
  geom_raster(interpolate=FALSE)+
  facet_wrap(~ Year,ncol=1)+
  scale_fill_manual(values=c(
    "-25% to 0%"="black",
    "+0% to 100%"="#440154FF",
    "+100% to 200%"="#3B528BFF",
    "+200% to 300%"="#21908CFF",
    "+300% to 400%"="#5DC863FF",
    ">400%"="#FDE725FF"))+
  guides(fill=guide_legend(title="%Δ from 2006"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks=c(1,4,7,10,13),labels=c("18-25","35-40","50-55","65-70","80-85"))+xlab("Age")+
  scale_y_continuous(limits=c(1,6))+ylab("Elixhauser Comorbidity Score")
  
p
ggsave('Figures/HeatMap.tiff',p,height=9/2,width=10/2,dpi=600)
  
  