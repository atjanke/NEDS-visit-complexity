library(readxl)





df <- read_excel("Observed-Expected-Change.xlsx") 

df %>%
  select(Year,Observed,Projections) %>%
  rename(Projected=Projections) %>%
  pivot_longer(Observed:Projected) %>%
  mutate(Modeling = case_when(
    Year<2015 ~"Using ICD-9/CCS Codes",
    Year>2015 ~"Using ICD-10/CCSR Codes"
  )) %>%
  rename(Trend=name) %>%
  ggplot(aes(x=Year,y=value,group=Trend,color=Trend))+
  geom_line(size=1)+
  geom_vline(xintercept=2014,linetype="dashed")+
  geom_vline(xintercept=2016,linetype="dashed")+
  scale_color_manual(values=c("Projected"="#440154FF","Observed"="#21908CFF"))+
  scale_y_continuous(limits=c(0,0.20),labels=scales::percent)+
  scale_x_continuous(breaks=c(2006,2009,2012,2015,2018))+
  ylab("Visits with High-Intensity Billing")+
  xlab("")+
  theme_bw()

df %>%
  mutate(Explained_Gain=Absolute_Gain-Unexplained_Gain) %>%
  select(Year,Unexplained_Gain:Explained_Gain) %>%
  pivot_longer(cols=Unexplained_Gain:Explained_Gain) %>%
  rename(Increase=name) %>%
  mutate(Increase = case_when(
    Increase=="Explained_Gain" ~ "Predicted by Case Mix Change",
    Increase=="Unexplained_Gain" ~ "Remaining"
  )) %>%
  mutate(Increase = case_when(
    Year==2015 & Increase=="Remaining" ~ "Not Modeled",
    Year==2016 & Increase=="Remaining" ~ "Not Modeled",
    T ~ Increase)) %>%
  mutate(value = case_when(
    Year==2015 & Increase=="Not Modeled" ~ 0.094,
    Year==2016 & Increase=="Not Modeled" ~ 0.125,
    T ~ value
  )) %>%
  ggplot(aes(fill=Increase,y=value,x=Year))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c("Not Modeled"="grey90","Predicted by Case Mix Change"="#440154FF","Remaining"="#FDE725FF"))+
  scale_y_continuous(labels=scales::percent_format())+
  scale_x_continuous(breaks=c(2006,2009,2012,2015,2018))+
  ylab("Year-Over-Year Percent Increase")+
  xlab("")+
  labs(fill="Growth in High-Intensity Billing: ")+
  theme_bw()