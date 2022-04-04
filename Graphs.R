#packages
library(dplyr)
library(ggplot2)

#Import database
setwd("C:/Users/jessica/OneDrive - University of Bath/Bioinformatics/Sam/Assessment 2/")
df<-read.csv(file = "Gram negative Oxidase positive AMR.csv")
df<-as.data.frame(df)

#Sum AMR
df2<-df %>%
  rowwise() %>%
  mutate(total=sum(c_across(18:34))) #add column with sum of AMR

#Year + Total AMR
df3<-df2[,c(10,35)]
df3<-df3 %>%
  group_by(year) %>%
  summarise(Abx=mean(total)) #average out AMR per year

#Plot scatter graph
ggplot(data=df3, aes(x=year, y=Abx)) +
  xlab("Year") +
  ylab("Level of Antibiotics Resistance") +
  geom_point() + #add points to graph
  geom_smooth(method=lm) #add trendline









