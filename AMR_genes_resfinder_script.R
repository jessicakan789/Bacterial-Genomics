#packages
library(data.table)
library(dplyr)
library(tidyverse)

#Import database
setwd("C:/Users/jessica/OneDrive - University of Bath/Bioinformatics/Sam/Assessment 2/Bordetella")
#d<- fread(".csv")
d<-read.csv(file = "Bordetella ALL.csv")
d<-as.data.frame(d)

#Convert genes columns to a binary variable
d[,-c(1:17)][d[,-c(1:17)]=="X"]<-"0" #17 is the number of columns that don't refer to gene presence
d[,-c(1:17)][d[,-c(1:17)]=="I"]<-"0"
d[,-c(1:17)][d[,-c(1:17)]!="0"]<-"1"
d[,-c(1:17)]<-sapply(d[,-c(1:17)],as.numeric)
#Classify according the family

#Aminoglycosides (selected according paper Van Hoek et al., 2011)
d <- d%>%
  rowwise() %>%
  mutate(amino = sum(c_across(18:283))) #18:283 to select the columns with aminoglycosides
d$amino[d$amino>0]<-1

#Betalactams
d <- d%>%
  rowwise() %>%
  mutate(betalactamics = sum(c_across(284:2293)))
d$betalactamics[d$betalactamics>0]<-1

#Colistin
d <- d%>%
  rowwise() %>%
  mutate(Colistin = sum(c_across(2294:2349)))
d$Colistin[d$Colistin>0]<-1        

#Disinfectants
d <- d%>%
  rowwise() %>%
  mutate(disinfectant = sum(c_across(2350:2365)))
d$disinfectant[d$disinfectant>0]<-1 

#Fosfomycin
d <- d%>%
  rowwise() %>%
  mutate(fosfomycin = sum(c_across(2366:2406)))
d$fosfomycin[d$fosfomycin>0]<-1

#Fusidic acid
d <- d%>%
  rowwise() %>%
  mutate(fusidic_acid = sum(c_across(2407:2409)))
d$fusidic_acid[d$fusidic_acid>0]<-1

#Glycopeptide
d <- d%>%
  rowwise() %>%
  mutate(glycopeptide = sum(c_across(2410:2453)))
d$glycopeptide[d$glycopeptide>0]<-1

#macrolide
d <- d%>%
  rowwise() %>%
  mutate(macrolide = sum(c_across(2454:2628)))
d$macrolide[d$macrolide>0]<-1

#nitroimidazole
d <- d%>%
  rowwise() %>%
  mutate(nitroimidazole = sum(c_across(2629:2642)))
d$nitroimidazole[d$nitroimidazole>0]<-1

#oxazolidinone
d <- d%>%
  rowwise() %>%
  mutate(oxazolidinone = sum(c_across(2643:2666)))
d$oxazolidinone[d$oxazolidinone>0]<-1

#phenicols
d <- d%>%
  rowwise() %>%
  mutate(phenicol = sum(c_across(2667:2714)))
d$phenicol[d$phenicol>0]<-1

#pseudomonic acid
d <- d%>%
  rowwise() %>%
  mutate(pseudomonic_acid = sum(c_across(2715:2717)))
d$pseudomonic_acid[d$pseudomonic_acid>0]<-1

#quinolone
d <- d%>%
  rowwise() %>%
  mutate(quinolone = sum(c_across(2718:2845)))
d$quinolone[d$quinolone>0]<-1

#rifampicin
d <- d%>%
  rowwise() %>%
  mutate(rifampicin = sum(c_across(2846:2855)))
d$rifampicin[d$rifampicin>0]<-1

#sulfonamides  
d <- d%>%
  rowwise() %>%
  mutate(sulfonamide = sum(c_across(2856:2909)))
d$sulfonamide[d$sulfonamide>0]<-1

#tetracyclines
d <- d%>%
  rowwise() %>%
  mutate(tetracycline = sum(c_across(2910:3056)))
d$tetracycline[d$tetracycline>0]<-1

#trimethoprim  
d <- d%>%
  rowwise() %>%
  mutate(trimethoprim = sum(c_across(3057:3164)))
d$trimethoprim[d$trimethoprim>0]<-1

#isolate grouped AMR only - 17 groups
d2 <- d[,c(1:17, 3165:3181)]

#Export the database to a csv file
write_csv2(d2,"Bordetella AMR.csv") #Important to add the extension and keep ""
