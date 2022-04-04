#packages
library(dplyr)
library(tidyverse)

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
  summarise(Abx=mean(total)/17*100) #average out AMR per year %

#Plot scatter graph
ggplot(data=df3, aes(x=year, y=Abx)) +
  xlab("Year") +
  ylab("Level of Antibiotics Resistance / %") +
  geom_point() + #add points to graph
  geom_smooth(method=lm) #add trendline

#get R^2 value
summary(lm(Abx~year, data=df3))

###################################################################################

#Split AMR into Bacteria and year
df4<-df2[,c(3,10,35)]
df4<-df4%>%
  group_by(Database, year) %>%
  summarize(Abx=mean(total)/17*100)

#Plot scatter graph
ggplot(df4, aes(x=year, y=Abx, color=Database)) +
  xlab("Year") +
  ylab("Level of Antibiotics Resistance / %") +
  geom_smooth(method=lm, se=FALSE) + #add trendline
  scale_color_discrete(name = NULL) #remove database title in legend

##################################################################################

#Split AMR into Abx and year
#Aminoglycoside
Aminoglycoside<-df2[,c(10,18)]
Aminoglycoside<-Aminoglycoside%>%
  group_by(year) %>%
  summarize(Abx=mean(amino)/17*100)

#Betalactam
Betalactam<-df2[,c(10,19)]
Betalactam<-Betalactam%>%
  group_by(year) %>%
  summarize(Abx=mean(betalactamics)/17*100)

#Plot scatter graph
ggplot() +
  xlab("Year") +
  ylab("Level of Antibiotics Resistance / %") +
  geom_point(data=Aminoglycoside, aes(x=year, y=Abx, color="red")) +
  geom_point(data=Betalactam, aes(x=year, y=Abx, color="orange")) + 
  scale_color_discrete(name = NULL, labels = c("Aminoglycoside", "Beta-lactam"))



###################################################################################

#Location

#Split AMR into Bacteria and location
df5<-df2[,c(6,35)]
df5<-df5%>%
  group_by(continent) %>%
  summarize(Abx=mean(total)/17*100)
df5<-df5[c(2:7),] #drop unspecified column

#Plot bar graph
ggplot(df5, aes(x=continent, y=Abx, fill=continent)) +
  xlab("Continent") +
  ylab("Level of Antibiotics Resistance / %") +
  geom_bar(stat="identity")

###################################################################################

#Host

#Split AMR into Bacteria and host
df6<-df2[,c(11,35)]
df6<-df6%>%
  group_by(source) %>%
  summarize(Abx=mean(total)/17*100)

#Plot bar graph
ggplot(df6, aes(x=source, y=Abx, fill=source)) +
  xlab("Host") +
  ylab("Level of Antibiotics Resistance / %") +
  geom_bar(stat="identity")


