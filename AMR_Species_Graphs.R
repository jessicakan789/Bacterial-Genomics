# Evolution of AMR across time for every species

# Install packages
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)

# Get data
setwd("C:/Users/jessica/OneDrive - University of Bath/Bioinformatics/Sam/Assessment 2/")
df <- read.csv(file = "Gram negative Oxidase positive AMR 2.csv")
df <- as.data.frame(df)

# Burkholderia cepacia complex
Burkholderia_cepacia_mean <- df[c(1:1407),] # subset data with rows containing B cepacia
Burkholderia_cepacia_mean <- Burkholderia_cepacia_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year and get percentage - 17 Abx classes

Burkholderia_cepacia_graphic <- ggplot(Burkholderia_cepacia_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Burkholderia cepacia") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,30) + 
  xlab("") + 
  ylab("")

# Burkholderia_pseudomallei
Burkholderia_pseudomallei_mean <- df[c(1408:2250),]
Burkholderia_pseudomallei_mean <- Burkholderia_pseudomallei_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Burkholderia_pseudomallei_graphic <- ggplot(Burkholderia_pseudomallei_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Burkholderia pseudomallei") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,10) + 
  xlab("") + 
  ylab("")

# Bordetella
Bordetella_mean <- df[c(2251:4335),]
Bordetella_mean <- Bordetella_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Bordetella_graphic <- ggplot(Bordetella_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Bordetella") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,10) + 
  xlab("") + 
  ylab("")

# Borrelia
Borrelia_mean <- df[c(4336:4439),]
Borrelia_mean <- Borrelia_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Borrelia_graphic <- ggplot(Borrelia_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Borrelia") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Brucella
Brucella_mean <- df[c(4440:4547),]
Brucella_mean <- Brucella_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Brucella_graphic <- ggplot(Brucella_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Brucella") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,2) + 
  xlab("") + 
  ylab("")

# Chlamydiales
Chlamydiales_mean <- df[c(4548:5250),]
Chlamydiales_mean <- Chlamydiales_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Chlamydiales_graphic <- ggplot(Chlamydiales_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Chlamydiales") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,10) + 
  xlab("") + 
  ylab("")

# Dichelobacter_nodosus
Dichelobacter_nodosus_mean <- df[c(5251:5423),]
Dichelobacter_nodosus_mean <- Dichelobacter_nodosus_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Dichelobacter_nodosus_graphic <- ggplot(Dichelobacter_nodosus_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Dichelobacter nodosus") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Glaesserella
Glaesserella_mean <- df[c(5424:5712),]
Glaesserella_mean <- Glaesserella_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Glaesserella_graphic <- ggplot(Glaesserella_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Glaesserella parasuis") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,5) + 
  xlab("") + 
  ylab("")

# Haemophilus_influenzae
Haemophilus_influenzae_mean <- df[c(5713:8344),]
Haemophilus_influenzae_mean <- Haemophilus_influenzae_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Haemophilus_influenzae_graphic <- ggplot(Haemophilus_influenzae_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Haemophilus influenzae") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,10) + 
  xlab("") + 
  ylab("")

# Helicobacter_pylori
Helicobacter_pylori_mean <- df[c(8345:8987),]
Helicobacter_pylori_mean <- Helicobacter_pylori_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Helicobacter_pylori_graphic <- ggplot(Helicobacter_pylori_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Helicobacter pylori") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Leptospira
Leptospira_mean <- df[c(8988:9749),]
Leptospira_mean <- Leptospira_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Leptospira_graphic <- ggplot(Leptospira_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Leptospira") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,2) + 
  xlab("") + 
  ylab("")

# Neisseria
Neisseria_mean <- df[c(9750:51085),]
Neisseria_mean <- Neisseria_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Neisseria_graphic <- ggplot(Neisseria_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Neisseria") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,10) + 
  xlab("") + 
  ylab("")

# Pseudomonas_aeruginosa
Pseudomonas_aeruginosa_mean <- df[c(51086:53774),]
Pseudomonas_aeruginosa_mean <- Pseudomonas_aeruginosa_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Pseudomonas_aeruginosa_graphic <- ggplot(Pseudomonas_aeruginosa_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Pseudomonas aeruginosa") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,50) + 
  xlab("") + 
  ylab("")

# Treponema
Treponema_mean <- df[c(53775:54376),]
Treponema_mean <- Treponema_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Treponema_graphic <- ggplot(Treponema_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Treponema pallidum") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Vibrio_cholerae
Vibrio_cholerae_mean <- df[c(54377:56040),]
Vibrio_cholerae_mean <- Vibrio_cholerae_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Vibrio_cholerae_graphic <- ggplot(Vibrio_cholerae_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Vibrio cholerae") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,30) + 
  xlab("") + 
  ylab("")

# Vibrio_parahaemolyticus
Vibrio_parahaemolyticus_mean <- df[c(56041:57932),]
Vibrio_parahaemolyticus_mean <- Vibrio_parahaemolyticus_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(year) %>%
  summarise(mean=mean(total)/17*100) # find mean of AMR by year

Vibrio_parahaemolyticus_graphic <- ggplot(Vibrio_parahaemolyticus_mean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Vibrio parahaemolyticus") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5, size=10)) + 
  geom_smooth() + 
  ylim(0,20) + 
  xlab("") + 
  ylab("")

#Joining the graphs
AMRclassestime <- ggarrange(Bordetella_graphic, Borrelia_graphic, Brucella_graphic, 
                            Burkholderia_cepacia_graphic, Burkholderia_pseudomallei_graphic, 
                            Chlamydiales_graphic, Dichelobacter_nodosus_graphic, 
                            Glaesserella_graphic, Haemophilus_influenzae_graphic, 
                            Helicobacter_pylori_graphic, Leptospira_graphic, 
                            Neisseria_graphic, Pseudomonas_aeruginosa_graphic, 
                            Treponema_graphic, Vibrio_cholerae_graphic, 
                            Vibrio_parahaemolyticus_graphic)

#Adding the title and the axis
annotate_figure(AMRclassestime, 
                #top=text_grob("Evolution of Gram-negative oxidase-positive bacteria AMR over time"), 
                bottom=text_grob("Year"), 
                left = text_grob("Level of resistance / %",rot = 90, vjust = 1))



