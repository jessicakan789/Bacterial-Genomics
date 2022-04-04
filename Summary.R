#packages
library(dplyr)

#Import database
setwd("C:/Users/jessica/OneDrive - University of Bath/Bioinformatics/Sam/Assessment 2/")
#d<- fread("ALL.csv")
df<-read.csv(file = "Gram negative Oxidase positive AMR.csv")
df<-as.data.frame(df)
df2<-df[,c(18:34)] #isolate AMR groups

#Burkholderia_cepacia
Burkholderia_cepacia <- colSums(df2[c(1:1407),]) #select the rows with Burkholderia_cepacia

#Burkholderia_pseudomallei
Burkholderia_pseudomallei <- colSums(df2[c(1408:2250),]) #select the rows with Burkholderia_pseudomallei

#Bordetella
Bordetella <- colSums(df2[c(2251:4335),]) #select the rows with Bordetella

#Borrelia
Borrelia <- colSums(df2[c(4336:4439),]) #select the rows with Borrelia

#Brucella
Brucella <- colSums(df2[c(4440:4547),]) #select the rows with Brucella

#Chlamydiales
Chlamydiales <- colSums(df2[c(4548:5250),]) #select the rows with Chlamydiales

#Dichelobacter_nosodus
Dichelobacter_nosodus <- colSums(df2[c(5251:5423),]) #select the rows with Dichelobacter_nosodus

#Glaesserella
Glaesserella <- colSums(df2[c(5424:5712),]) #select the rows with Glaesserella

#Haemophilus influenzae
Haemophilus_influenzae <- colSums(df2[c(5713:8344),]) #select the rows with Haemophilus influenzae

#Helicobacter pylori
Helicobacter_pylori <- colSums(df2[c(8345:8987),]) #select the rows with Helicobacter pylori

#Leptospira
Leptospira <- colSums(df2[c(8988:9749),]) #select the rows with Leptospira

#Neisseria
Neisseria <- colSums(df2[c(9750:51085),]) #select the rows with Neisseria

#Pseudomonas aeruginosa
Pseudomonas_aeruginosa <- colSums(df2[c(51086:53774),]) #select the rows with Pseudomonas aeruginosa

#Treponema
Treponema <- colSums(df2[c(53775:54376),]) #select the rows with Treponema

#Vibrio cholerae
Vibrio_cholerae <- colSums(df2[c(54377:56040),]) #select the rows with Vibrio cholerae

#Vibrio parahaemolyticus
Vibrio_parahaemolyticus <- colSums(df2[c(56041:57932),]) #select the rows with Vibrio parahaemolyticus

#create dataframe
df3<-rbind(Bordetella, Borrelia, Brucella, Burkholderia_cepacia, Burkholderia_pseudomallei, 
           Chlamydiales, Dichelobacter_nosodus, Glaesserella, Haemophilus_influenzae, 
           Helicobacter_pylori, Leptospira, Neisseria, Pseudomonas_aeruginosa, 
           Treponema, Vibrio_cholerae, Vibrio_parahaemolyticus)
df3<-as.data.frame(df3)
df3[df3>0]<-1

#Export the database to a csv file
write.csv(df3,"Gram negative Oxidase positive Summary.csv")


