# Evolution of AMR across time for every AMR class

# Install packages
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)

# Get data
setwd("C:/Users/jessica/OneDrive - University of Bath/Bioinformatics/Sam/Assessment 2/")
df <- read.csv(file = "Gram negative Oxidase positive AMR 2.csv")
df <- as.data.frame(df)

# Aminoglycosides 
aminomean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(amino, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

aminographic <- ggplot(aminomean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Aminoglycosides") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ylim(0,20) + 
  xlab("") + 
  ylab("")

# Beta-lactams
betamean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(betalactamics, exclude.NA=T/1*100))

betagraphic <- ggplot(betamean, aes(year,mean)) + 
  geom_point() + 
  ggtitle("Betalactams") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Betalactamics") + 
  geom_smooth() + 
  ylim(0,2) +
  xlab("") + 
  ylab("")

# Colistin
colistinmean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(Colistin, exclude.NA=T/1*100))

colistingraphic <- ggplot(colistinmean, aes(year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ggtitle("Colistin") + 
  geom_smooth() + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Disinfectants
disinfectantmean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(disinfectant, exclude.NA=T/1*100))

disinfectantgraphic <- ggplot(disinfectantmean, aes(year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ggtitle("Disinfectants") + 
  geom_smooth() + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Fosfomycin
fosfomean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(fosfomycin, exclude.NA=T/1*100))

fosfographic <- ggplot(fosfomean, aes(year,mean))+geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ggtitle("Fosfomycin") + 
  geom_smooth() + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Fusidic acid
fusidicmean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(fusidic_acid, exclude.NA=T/1*100))

fusidicgraphic <- ggplot(fusidicmean, aes(year,mean))+geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ggtitle("Fusidic acid") + 
  geom_smooth() + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Glycopeptides
glycomean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(glycopeptide, exclude.NA=T/1*100))

glycographic <- ggplot(glycomean, aes(year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Glycopeptides") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Macrolides
macromean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(macrolide, exclude.NA=T/1*100))

macrographic <- ggplot(macromean, aes(year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Macrolides") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Nitroimidazole
nitromean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(nitroimidazole, exclude.NA=T/1*100))

nitrographic <- ggplot(nitromean, aes(year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Nitroimidazole") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Oxazolidinone
oxazmean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(oxazolidinone, na.rm=T, exclude.NA=T/1*100))

oxazgraphic <- ggplot(oxazmean, aes(year,mean))+geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Oxazolidinone") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Phenicol
phemean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(phenicol, na.rm=T, exclude.NA=T/1*100))

phegraphic <- ggplot(phemean, aes(year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Phenicols") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Pseudomonic acid
pseudomean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(pseudomonic_acid, na.rm=T, exclude.NA=T/1*100))

pseudographic <- ggplot(pseudomean, aes(year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Pseudomonic acid") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Quinolones
quinomean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(quinolone, na.rm=T, exclude.NA=T/1*100))

quinographic <- ggplot(quinomean, aes(year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Quinolones") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Rifampicin
rifamean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(rifampicin, na.rm=T, exclude.NA=T/1*100))

rifagraphic <- ggplot(rifamean, aes(year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Rifampicin") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Sulphonamides
sulfamean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(sulfonamide, na.rm=T, exclude.NA=T/1*100))

sulfagraphic <- ggplot(sulfamean, aes(year,mean)) + 
  geom_point() + 
  geom_smooth() + 
  ggtitle("Sulfonamides") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Tetracyclines
tetmean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(tetracycline, na.rm=T, exclude.NA=T/1*100))

tetgraphic <- ggplot(tetmean, aes(year,mean)) + 
  geom_point() + 
  geom_smooth() + 
  ggtitle("Tetracyclines") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Trimethoprim
trimean <- df %>%
  group_by(year)%>%
  summarise(mean=mean(trimethoprim, na.rm=T, exclude.NA=T/1*100))

trigraphic <- ggplot(trimean, aes(year,mean)) + 
  geom_point() + 
  geom_smooth() + 
  ggtitle("Trimethoprim") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

#Joining the graphs - excluded disinfectants as cannot fit
AMRclassestime<-ggarrange(aminographic,betagraphic,colistingraphic,fosfographic,
                          fusidicgraphic,glycographic,macrographic,nitrographic,oxazgraphic,phegraphic,
                          pseudographic,quinographic,rifagraphic,sulfagraphic,tetgraphic,trigraphic)

#Adding the title and the axis
annotate_figure(AMRclassestime, 
                #top=text_grob("Evolution of AMR genes families across time"), 
                bottom=text_grob("Year"), 
                left = text_grob("Level of resistance %",rot = 90, vjust = 1))



