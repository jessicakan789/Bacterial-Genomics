#packages
library(dplyr)
library(tidyverse)
library(reshape2)
library(lessR)
library(plotly)

#Import database
setwd("C:/Users/jessica/OneDrive - University of Bath/Bioinformatics/Sam/Assessment 2/")
df <- read.csv(file = "Gram negative Oxidase positive AMR 2.csv")
df <- as.data.frame(df)

#Sum AMR
df2 <- df %>%
  rowwise() %>%
  mutate(total=sum(c_across(18:34))) #add column with total AMR

#################################################################################

# Overall level of AMR resistance

#Subset Year + Total AMR
df3 <- df2[,c(10,35)]
df3 <- df3 %>%
  group_by(year) %>%
  summarise(AMR=mean(total)/17*100) #average out AMR per year %

#Plot scatter graph
ggplot(data=df3, aes(x=year, y=AMR)) +
  xlab("Year") +
  ylab("Level of Antibiotics Resistance / %") +
  geom_point() + #add points to graph
  geom_smooth(method=lm) #add trendline

#get R^2 value
summary(lm(AMR~year, data=df3))

###################################################################################

# Split AMR into Bacteria and year

#subset data
df4 <- df2[,c(3,10,35)]
df4 <- df4 %>%
  group_by(Database, year) %>% # Database = bacterial species
  summarize(AMR=mean(total)/17*100)

#Plot scatter graph
ggplot(df4, aes(x=year, y=AMR, color=Database)) +
  xlab("Year") +
  ylab("Level of Antibiotics Resistance / %") +
  geom_smooth(method=lm, se=FALSE) + #add trendline
  scale_color_discrete(name = NULL) + #remove database title in legend
  geom_point() # graph looks messy with points

##################################################################################

# Split AMR into Abx and year

#aminoglycoside
aminoglycoside <- df2[,c(10,18)] # select year and Abx columns
aminoglycoside <- aminoglycoside %>%
  group_by(year) %>%
  summarize(aminoglycoside=mean(amino)/17*100)

#betalactam
betalactam <- df2[,c(10,19)]
betalactam <- betalactam %>%
  group_by(year) %>%
  summarize(betalactam=mean(betalactamics)/17*100)

#colistin
colistin <- df2[,c(10,20)]
colistin <- colistin %>%
  group_by(year) %>%
  summarize(colistin=mean(Colistin)/17*100)

#disinfectants
disinfectant <- df2[,c(10,21)]
disinfectant <- disinfectant %>%
  group_by(year) %>%
  summarize(disinfectants=mean(disinfectant)/17*100)

#fosfomycin
fosfomycin <- df2[,c(10,22)]
fosfomycin <- fosfomycin %>%
  group_by(year) %>%
  summarize(Fosfomycin=mean(fosfomycin)/17*100)

#fusidic_acid
fusidic_acid <- df2[,c(10,23)]
fusidic_acid <- fusidic_acid %>%
  group_by(year) %>%
  summarize(fusidic_acid=mean(fusidic_acid)/17*100)

#glycopeptide
glycopeptide <- df2[,c(10,24)]
glycopeptide <- glycopeptide %>%
  group_by(year) %>%
  summarize(glycopeptide=mean(glycopeptide)/17*100)

#macrolide
macrolide <- df2[,c(10,25)]
macrolide <- macrolide %>%
  group_by(year) %>%
  summarize(macrolide=mean(macrolide)/17*100)

#nitroimidazole
nitroimidazole <- df2[,c(10,26)]
nitroimidazole <- nitroimidazole %>%
  group_by(year) %>%
  summarize(nitroimidazole=mean(nitroimidazole)/17*100)

#oxazolidinone
oxazolidinone <- df2[,c(10,27)]
oxazolidinone <- oxazolidinone %>%
  group_by(year) %>%
  summarize(oxazolidinone=mean(oxazolidinone)/17*100)

#phenicol
phenicol <- df2[,c(10,28)]
phenicol <- phenicol %>%
  group_by(year) %>%
  summarize(phenicol=mean(phenicol)/17*100)

#pseudomonic_acid
pseudomonic_acid <- df2[,c(10,29)]
pseudomonic_acid <- pseudomonic_acid %>%
  group_by(year) %>%
  summarize(pseudomonic_acid=mean(pseudomonic_acid)/17*100)

#quinolone
quinolone <- df2[,c(10,30)]
quinolone <- quinolone %>%
  group_by(year) %>%
  summarize(quinolone=mean(quinolone)/17*100)

#rifampicin
rifampicin <- df2[,c(10,31)]
rifampicin <- rifampicin %>%
  group_by(year) %>%
  summarize(rifampicin=mean(rifampicin)/17*100)

#sulfonamide
sulfonamide <- df2[,c(10,32)]
sulfonamide <- sulfonamide %>%
  group_by(year) %>%
  summarize(sulfonamide=mean(sulfonamide)/17*100)

#tetracycline
tetracycline <- df2[,c(10,33)]
tetracycline <- tetracycline %>%
  group_by(year) %>%
  summarize(tetracycline=mean(tetracycline)/17*100)

#trimethoprim
trimethoprim <- df2[,c(10,34)]
trimethoprim <- trimethoprim %>%
  group_by(year) %>%
  summarize(trimethoprim=mean(trimethoprim)/17*100)

#put all data frames into list
df_list <- list(aminoglycoside, betalactam, colistin, disinfectant, fosfomycin, fusidic_acid,
                glycopeptide, macrolide, nitroimidazole, oxazolidinone, phenicol, pseudomonic_acid,
                quinolone, rifampicin, sulfonamide, tetracycline, trimethoprim)

#merge all data frames in list
df_list <- df_list %>% reduce(full_join, by='year')

#convert list to dataframe
df5 <- as.data.frame(df_list)

#melt data frame into long format
df5 <- melt(df5 ,  id.vars = 'year', variable.name = 'series')

#Plot scatter graph
ggplot(df5, aes(year, value, colour = series)) +
  xlab("Year") +
  ylab("Level of Antibiotics Resistance / %") +
  scale_color_discrete(name = NULL) + 
  geom_smooth(method=lm, se=FALSE) + # add trendline
  geom_point()


###################################################################################

#Location

#Split AMR by location
df6 <- df2[,c(6,35)]
df6 <- df6 %>%
  group_by(continent) %>%
  summarize(AMR=mean(total)/17*100)

df6 <- df6[c(2:7),] # drop unspecified column

#Plot bar graph
ggplot(df6, aes(x=continent, y=AMR, fill=continent)) +
  xlab("Continent") +
  ylab("Level of Antibiotics Resistance / %") +
  geom_bar(stat="identity")

###################################################################################

#Host

#Split AMR by host
df7 <- df2[,c(11,35)]
df7 <- df7 %>%
  group_by(source) %>%
  summarize(AMR=mean(total)/17*100)

df7 <- df7[c(2:6),] # drop unspecified row

#Plot bar graph
ggplot(df7, aes(x=source, y=AMR, fill=source)) +
  xlab("Host") +
  ylab("Level of Antibiotics Resistance / %") +
  geom_bar(stat="identity")

#################################################################################

# some pie charts summarizing the data

# Years
df8 <- df[,c(3,10)] # subset data
df8$year[is.na(df8$year)]<-"Unspecified" # change blanks to "Unspecified" so we get a label on the pie

PieChart(year, hole = 0, values = "%", data = df8, main="")

# Location
df9 <- df[,c(3,6)] # subset data
df9$continent[df9$continent==""]<-"Unspecified" # change blanks to "Unspecified" so we get a label on the pie

PieChart(continent, hole = 0, values = "%", data = df9, main="")

# Host
df10 <- df[,c(3,11)] # subset data
df10$source[df10$source==""]<-"Unspecified" # change blanks to "Unspecified" so we get a label on the pie

PieChart(source, hole = 0, values = "%", data = df10, main="")

#################################################################################

# Choropleth map graph

#Split AMR by location
df11 <- df2[,c(5,35)]
df11 <- df11 %>%
  group_by(country) %>%
  summarize(AMR=mean(total)/17*100)

df11 <- df11[-c(1:5),] # drop unspecified and continent rows

# sort out UK
UK <- sum(df11[c(133:137),2]) # sum AMR rows
new_row <- c("UK", UK)
df11 <- rbind(df11, new_row) # add new row to data frame
df11 <- df11[-c(133:137),] # drop unwanted rows
write.csv(df11, file="Country_AMR.csv") # write to CSV file

# add country ISO codes to data frame
df11$codes <- c("AFG","ALB","DZA","AGO","ARG","ARM","AUS","AUT","BHR","BGD","BRB","BLR",
                "BEL","BEN","BTN","BIH","BWA","BRA","BGR","BFA","BUR","CAN","KHM","CMR",
                "CPV","RCA","TCD","CHN","HKG","CHL","COL","COD","COG","CRI","HRV","CUB",
                "CYP","CZE","DNK","DJI","DOM","ECU","EGY","EST","ETH","FJI","FIN","FRA",
                "DEU","GHA","GRC","GLP","GTM","GIN","GNB","HTI","HND","HUN","ISL","IND",
                "IDN","IRN","IRQ","IRL","IMN","ISR","ITA","CIV","JAM","JPN","KAZ","KEN",
                "LAO","LVA","LBN","LBR","LTU","MDG","MWI","MYS","MLI","MLT","MUS","MYT",
                "MEX","NAM","MAR","MOZ","NPL","NCL","NZL","NIC","NER","NGA","NOR","PAK",
                "PAN","PNG","PER","PHL","POL","PRT","PRI","ROU","RUS","WSM","SAU","SEN",
                "SGP","SVK","SVN","SLB","SOM","ZAF","KOR","ESP","LKA","SDN","SUR","SWE",
                "CHE","SYR","TAI","TZA","THA","GMB","NLD","TGO","TTO","TUN","TUR","UGA",
                "UKR","URY","USA","UZB","VEN","VNM","YEM","ZMB","ZWE","GBR")

plot_ly(df11, type='choropleth', locations=df11$codes, z=df11$AMR)


