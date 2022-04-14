#packages
library(dplyr)

#Import database
setwd("C:/Users/jessica/OneDrive - University of Bath/Bioinformatics/Sam/Assessment 2/")
df<-read.csv(file = "Gram negative Oxidase positive AMR.csv")
df<-as.data.frame(df)
df2<-df[,c(18:34)] #isolate AMR groups

#Burkholderia_cepacia
Burkholderia_cepacia <- colMeans(df2[c(1:1407),])*100 #select the rows with Burkholderia_cepacia
                                                      #get percentage of isolates resistant to each Abx

#Burkholderia_pseudomallei
Burkholderia_pseudomallei <- colMeans(df2[c(1408:2250),])*100 #select the rows with Burkholderia_pseudomallei

#Bordetella
Bordetella <- colMeans(df2[c(2251:4335),])*100 #select the rows with Bordetella

#Borrelia
Borrelia <- colMeans(df2[c(4336:4439),])*100 #select the rows with Borrelia

#Brucella
Brucella <- colMeans(df2[c(4440:4547),])*100 #select the rows with Brucella

#Chlamydiales
Chlamydiales <- colMeans(df2[c(4548:5250),])*100 #select the rows with Chlamydiales

#Dichelobacter_nosodus
Dichelobacter_nosodus <- colMeans(df2[c(5251:5423),])*100 #select the rows with Dichelobacter_nosodus

#Glaesserella
Glaesserella <- colMeans(df2[c(5424:5712),])*100 #select the rows with Glaesserella

#Haemophilus influenzae
Haemophilus_influenzae <- colMeans(df2[c(5713:8344),])*100 #select the rows with Haemophilus influenzae

#Helicobacter pylori
Helicobacter_pylori <- colMeans(df2[c(8345:8987),])*100 #select the rows with Helicobacter pylori

#Leptospira
Leptospira <- colMeans(df2[c(8988:9749),])*100 #select the rows with Leptospira

#Neisseria
Neisseria <- colMeans(df2[c(9750:51085),])*100 #select the rows with Neisseria

#Pseudomonas aeruginosa
Pseudomonas_aeruginosa <- colMeans(df2[c(51086:53774),])*100 #select the rows with Pseudomonas aeruginosa

#Treponema
Treponema <- colMeans(df2[c(53775:54376),])*100 #select the rows with Treponema

#Vibrio cholerae
Vibrio_cholerae <- colMeans(df2[c(54377:56040),])*100 #select the rows with Vibrio cholerae

#Vibrio parahaemolyticus
Vibrio_parahaemolyticus <- colMeans(df2[c(56041:57932),])*100 #select the rows with Vibrio parahaemolyticus

#create dataframe
df3<-rbind(Bordetella, Borrelia, Brucella, Burkholderia_cepacia, Burkholderia_pseudomallei, 
           Chlamydiales, Dichelobacter_nosodus, Glaesserella, Haemophilus_influenzae, 
           Helicobacter_pylori, Leptospira, Neisseria, Pseudomonas_aeruginosa, 
           Treponema, Vibrio_cholerae, Vibrio_parahaemolyticus)
df3<-as.data.frame(df3)

#Export the database to a csv file
write.csv(df3,"Gram negative Oxidase positive Summary 2.csv")


