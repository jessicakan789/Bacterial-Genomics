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
aminomean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(amino, exclude.NA=T))

aminographic<-ggplot(aminomean, aes(year,mean))+geom_point()+ggtitle("Aminoglycosides")+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+geom_smooth()+ylim(0,1)+xlab("")+ylab("")

# Beta-lactams
betamean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(betalactamics, exclude.NA=T))

betagraphic<-ggplot(betamean, aes(year,mean))+geom_point()+ggtitle("Betalactamics")+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+labs("Betalactamics")+geom_smooth()+ylim(0,1)+xlab("")+ylab("")

# Colistin
colistinmean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(Colistin, exclude.NA=T))

colistingraphic<-ggplot(colistinmean, aes(year,mean))+geom_point()+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+ggtitle("Colistin")+geom_smooth()+ylim(0,1)+xlab("")+ylab("")

# Fosfomycin
fosfomean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(fosfomycin, exclude.NA=T))

fosfographic<-ggplot(fosfomean, aes(year,mean))+geom_point()+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+ggtitle("Fosfomycin")+geom_smooth()+ylim(0,1)+xlab("")+ylab("")

# Glycopeptides
glycomean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(glycopeptide, exclude.NA=T))

glycographic<-ggplot(glycomean, aes(year,mean))+geom_point()+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+geom_smooth()+ggtitle("Glycopeptides")+ylim(0,1)+xlab("")+ylab("")

# Macrolides
macromean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(macrolide, exclude.NA=T))

macrographic<-ggplot(macromean, aes(year,mean))+geom_point()+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+geom_smooth()+ggtitle("Macrolides")+ylim(0,1)+xlab("")+ylab("")

# Oxazolidinone
oxazmean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(oxazolidinone, na.rm=T, exclude.NA=T))

oxazgraphic<-ggplot(oxazmean, aes(year,mean))+geom_point()+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+geom_smooth()+ggtitle("Oxazolidinone")+ylim(0,1)+xlab("")+ylab("")

# Phenicol
phemean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(phenicol, na.rm=T, exclude.NA=T))

phegraphic<-ggplot(phemean, aes(year,mean))+geom_point()+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+geom_smooth()+ggtitle("Phenicols")+ylim(0,1)+xlab("")+ylab("")

# Quinolones
quinomean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(quinolone, na.rm=T, exclude.NA=T))

quinographic<-ggplot(quinomean, aes(year,mean))+geom_point()+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+geom_smooth()+ggtitle("Quinolones")+ylim(0,1)+xlab("")+ylab("")

# Rifampicin
rifamean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(rifampicin, na.rm=T, exclude.NA=T))

rifagraphic<-ggplot(rifamean, aes(year,mean))+geom_point()+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+geom_smooth()+ggtitle("Rifampicin")+ylim(0,1)+xlab("")+ylab("")

# Sulphonamides
sulfamean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(sulfonamide, na.rm=T, exclude.NA=T))

sulfagraphic<-ggplot(sulfamean, aes(year,mean))+geom_point()+geom_smooth()+ggtitle("Sulfonamides")+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+ylim(0,1)+xlab("")+ylab("")

# Tetracyclines
tetmean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(tetracycline, na.rm=T, exclude.NA=T))

tetgraphic<-ggplot(tetmean, aes(year,mean))+geom_point()+geom_smooth()+ggtitle("Tetracyclines")+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+ylim(0,1)+xlab("")+ylab("")

# Trimethoprim
trimean<-df%>%
  group_by(year)%>%
  summarise(mean=mean(trimethoprim, na.rm=T, exclude.NA=T))

trigraphic<-ggplot(trimean, aes(year,mean))+geom_point()+geom_smooth()+ggtitle("Trimethoprim")+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+ylim(0,1)+xlab("")+ylab("")

#Joining the graphs
AMRclassestime<-ggarrange(aminographic,betagraphic,colistingraphic, fosfographic,glycographic, macrographic,oxazgraphic,phegraphic,quinographic,rifagraphic, sulfagraphic,tetgraphic,trigraphic)

#Adding the title and the axis
annotate_figure(AMRclassestime, top=text_grob("Evolution of AMR genes families across time"), bottom=text_grob("Year"), left = text_grob("Percentage of resistance",rot = 90, vjust = 1))

#Number of AMR classes
nummean<-df%>%
  group_by(year)%>%     
  summarise(mean=mean(numclass, na.rm=T, exclude.NA=T))

numgraphic<-ggplot(nummean, aes(year,mean))+geom_point()+geom_smooth()+ggtitle("Number of AMR classes")+theme(plot.title=element_text( hjust=0.5, vjust=0.5))+xlab("")+ylab("")+ylim(0,13)


