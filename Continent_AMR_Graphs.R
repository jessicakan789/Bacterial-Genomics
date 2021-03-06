#packages
library(dplyr)
library(tidyverse)
library(reshape2)
library(lessR)
library(plotly)
library(ggplot2)
library(ggpubr)

#Import database
setwd("C:/Users/jessica/OneDrive - University of Bath/Bioinformatics/Sam/Assessment 2/")
df <- read.csv(file = "Gram negative Oxidase positive AMR 2.csv")
df <- as.data.frame(df)

#################################################################################

# Continent by bacteria

# Burkholderia cepacia complex
Burkholderia_cepacia_mean <- df[c(1:1407),] # subset data with rows containing B cepacia
Burkholderia_cepacia_mean <- Burkholderia_cepacia_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Burkholderia_cepacia_mean <- Burkholderia_cepacia_mean[-1,] # Get rid of unspecified row

Burkholderia_cepacia_mean <- Burkholderia_cepacia_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Burkholderia_cepacia_graphic <- ggplot(Burkholderia_cepacia_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Burkholderia cepacia")

# Burkholderia_pseudomallei
Burkholderia_pseudomallei_mean <- df[c(1408:2250),]
Burkholderia_pseudomallei_mean <- Burkholderia_pseudomallei_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Burkholderia_pseudomallei_mean <- Burkholderia_pseudomallei_mean[-1,] # Get rid of unspecified row

Burkholderia_pseudomallei_mean <- Burkholderia_pseudomallei_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Burkholderia_pseudomallei_graphic <- ggplot(Burkholderia_pseudomallei_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Burkholderia pseudomallei")

# Bordetella
Bordetella_mean <- df[c(2251:4335),]
Bordetella_mean <- Bordetella_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Bordetella_mean <- Bordetella_mean[-1,] # Get rid of unspecified row

Bordetella_mean <- Bordetella_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Bordetella_graphic <- ggplot(Bordetella_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Bordetella")

# Borrelia
Borrelia_mean <- df[c(4336:4439),]
Borrelia_mean <- Borrelia_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Borrelia_mean <- Borrelia_mean[-1,] # Get rid of unspecified row

Borrelia_mean <- Borrelia_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Borrelia_graphic <- ggplot(Borrelia_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Borrelia") 

# Brucella
Brucella_mean <- df[c(4440:4547),]
Brucella_mean <- Brucella_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Brucella_mean <- Brucella_mean[-1,] # Get rid of unspecified row

Brucella_mean <- Brucella_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Brucella_graphic <- ggplot(Brucella_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Brucella")

# Chlamydiales
Chlamydiales_mean <- df[c(4548:5250),]
Chlamydiales_mean <- Chlamydiales_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Chlamydiales_mean <- Chlamydiales_mean[-1,] # Get rid of unspecified row

Chlamydiales_mean <- Chlamydiales_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Chlamydiales_graphic <- ggplot(Chlamydiales_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Chlamydiales")

# Dichelobacter_nodosus
Dichelobacter_nodosus_mean <- df[c(5251:5423),]
Dichelobacter_nodosus_mean <- Dichelobacter_nodosus_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Dichelobacter_nodosus_mean <- Dichelobacter_nodosus_mean[-1,] # Get rid of unspecified row

Dichelobacter_nodosus_mean <- Dichelobacter_nodosus_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Dichelobacter_nodosus_graphic <- ggplot(Dichelobacter_nodosus_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Dichelobacter nodosus")

# Glaesserella
Glaesserella_mean <- df[c(5424:5712),]
Glaesserella_mean <- Glaesserella_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Glaesserella_mean <- Glaesserella_mean[-1,] # Get rid of unspecified row

Glaesserella_mean <- Glaesserella_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Glaesserella_graphic <- ggplot(Glaesserella_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Glaesserella parasuis")

# Haemophilus_influenzae
Haemophilus_influenzae_mean <- df[c(5713:8344),]
Haemophilus_influenzae_mean <- Haemophilus_influenzae_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Haemophilus_influenzae_mean <- Haemophilus_influenzae_mean[-1,] # Get rid of unspecified row

Haemophilus_influenzae_mean <- Haemophilus_influenzae_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Haemophilus_influenzae_graphic <- ggplot(Haemophilus_influenzae_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Haemophilus influenzae")

# Helicobacter_pylori
Helicobacter_pylori_mean <- df[c(8345:8987),]
Helicobacter_pylori_mean <- Helicobacter_pylori_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Helicobacter_pylori_mean <- Helicobacter_pylori_mean[-1,] # Get rid of unspecified row

Helicobacter_pylori_mean <- Helicobacter_pylori_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Helicobacter_pylori_graphic <- ggplot(Helicobacter_pylori_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Helicobacter pylori")

# Leptospira
Leptospira_mean <- df[c(8988:9749),]
Leptospira_mean <- Leptospira_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Leptospira_mean <- Leptospira_mean[-1,] # Get rid of unspecified row

Leptospira_mean <- Leptospira_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Leptospira_graphic <- ggplot(Leptospira_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Leptospira")

# Neisseria
Neisseria_mean <- df[c(9750:51085),]
Neisseria_mean <- Neisseria_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Neisseria_mean <- Neisseria_mean[-1,] # Get rid of unspecified row

Neisseria_mean <- Neisseria_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Neisseria_graphic <- ggplot(Neisseria_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Neisseria")

# Pseudomonas_aeruginosa
Pseudomonas_aeruginosa_mean <- df[c(51086:53774),]
Pseudomonas_aeruginosa_mean <- Pseudomonas_aeruginosa_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Pseudomonas_aeruginosa_mean <- Pseudomonas_aeruginosa_mean[-1,] # Get rid of unspecified row

Pseudomonas_aeruginosa_mean <- Pseudomonas_aeruginosa_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Pseudomonas_aeruginosa_graphic <- ggplot(Pseudomonas_aeruginosa_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Pseudomonas aeruginosa")

# Treponema
Treponema_mean <- df[c(53775:54376),]
Treponema_mean <- Treponema_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Treponema_mean <- Treponema_mean[-1,] # Get rid of unspecified row

Treponema_mean <- Treponema_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Treponema_graphic <- ggplot(Treponema_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Treponema pallidum") 

# Vibrio_cholerae
Vibrio_cholerae_mean <- df[c(54377:56040),]
Vibrio_cholerae_mean <- Vibrio_cholerae_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Vibrio_cholerae_mean <- Vibrio_cholerae_mean[-1,] # Get rid of unspecified row

Vibrio_cholerae_mean <- Vibrio_cholerae_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Vibrio_cholerae_graphic <- ggplot(Vibrio_cholerae_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Vibrio cholerae")

# Vibrio_parahaemolyticus
Vibrio_parahaemolyticus_mean <- df[c(56041:57932),]
Vibrio_parahaemolyticus_mean <- Vibrio_parahaemolyticus_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Vibrio_parahaemolyticus_mean <- Vibrio_parahaemolyticus_mean[-1,] # Get rid of unspecified row

Vibrio_parahaemolyticus_mean <- Vibrio_parahaemolyticus_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Vibrio_parahaemolyticus_graphic <- ggplot(Vibrio_parahaemolyticus_mean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Vibrio parahaemolyticus")

#Joining the graphs
Continent_AMR_Species <- ggarrange(Bordetella_graphic, Borrelia_graphic, Brucella_graphic, 
                            Burkholderia_cepacia_graphic, Burkholderia_pseudomallei_graphic, 
                            Chlamydiales_graphic, Dichelobacter_nodosus_graphic, 
                            Glaesserella_graphic, Haemophilus_influenzae_graphic, 
                            Helicobacter_pylori_graphic, Leptospira_graphic, 
                            Neisseria_graphic, Pseudomonas_aeruginosa_graphic, 
                            Treponema_graphic, Vibrio_cholerae_graphic, 
                            Vibrio_parahaemolyticus_graphic)

#Adding the title and the axis
annotate_figure(Continent_AMR_Species, 
                #top=text_grob("Gram-negative oxidase-positive bacteria AMR"), 
                bottom=text_grob("Continent"), 
                left = text_grob("Level of resistance / %",rot = 90, vjust = 1))



#################################################################################

# Continent by antimicrobial class

# Aminoglycosides 
aminomean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(amino, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

aminomean <- aminomean[-1,] # Get rid of unspecified row

aminomean <- aminomean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

aminographic <- ggplot(aminomean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Aminoglycosides")

# Beta-lactams
betamean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(betalactamics, exclude.NA=T)/1*100)

betamean <- betamean[-1,] # Get rid of unspecified row

betamean <- betamean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

betagraphic <- ggplot(betamean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Beta-lactams")

# Colistin
colistinmean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(Colistin, exclude.NA=T)/1*100)

colistinmean <- colistinmean[-1,] # Get rid of unspecified row

colistinmean <- colistinmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

colistingraphic <- ggplot(colistinmean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Colistin")

# Disinfectants
disinfectantmean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(disinfectant, exclude.NA=T)/1*100)

disinfectantmean <- disinfectantmean[-1,] # Get rid of unspecified row

disinfectantmean <- disinfectantmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

disinfectantgraphic <- ggplot(disinfectantmean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Disinfectants")

# Fosfomycin
fosfomean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(fosfomycin, exclude.NA=T)/1*100)

fosfomean <- fosfomean[-1,] # Get rid of unspecified row

fosfomean <- fosfomean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

fosfographic <- ggplot(fosfomean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Fosfomycin")

# Fusidic acid
fusidicmean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(fusidic_acid, exclude.NA=T)/1*100)

fusidicmean <- fusidicmean[-1,] # Get rid of unspecified row

fusidicmean <- fusidicmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

fusidicgraphic <- ggplot(fusidicmean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Fusidic acid")

# Glycopeptides
glycomean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(glycopeptide, exclude.NA=T)/1*100)

glycomean <- glycomean[-1,] # Get rid of unspecified row

glycomean <- glycomean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

glycographic <- ggplot(glycomean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Glycopeptides")

# Macrolides
macromean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(macrolide, exclude.NA=T)/1*100)

macromean <- macromean[-1,] # Get rid of unspecified row

macromean <- macromean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

macrographic <- ggplot(macromean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Macrolides")

# Nitroimidazole
nitromean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(nitroimidazole, exclude.NA=T)/1*100)

nitromean <- nitromean[-1,] # Get rid of unspecified row

nitromean <- nitromean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

nitrographic <- ggplot(nitromean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Nitroimidazole")

# Oxazolidinone
oxazmean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(oxazolidinone, na.rm=T, exclude.NA=T)/1*100)

oxazmean <- oxazmean[-1,] # Get rid of unspecified row

oxazmean <- oxazmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

oxazgraphic <- ggplot(oxazmean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Oxazolidinone")

# Phenicol
phemean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(phenicol, na.rm=T, exclude.NA=T)/1*100)

phemean <- phemean[-1,] # Get rid of unspecified row

phemean <- phemean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

phegraphic <- ggplot(phemean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Phenicol")

# Pseudomonic acid
pseudomean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(pseudomonic_acid, na.rm=T, exclude.NA=T)/1*100)

pseudomean <- pseudomean[-1,] # Get rid of unspecified row

pseudomean <- pseudomean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

pseudographic <- ggplot(pseudomean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Pseudomonic acid")

# Quinolones
quinomean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(quinolone, na.rm=T, exclude.NA=T)/1*100)

quinomean <- quinomean[-1,] # Get rid of unspecified row

quinomean <- quinomean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

quinographic <- ggplot(quinomean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Quinolones")

# Rifamycin
rifamean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(rifampicin, na.rm=T, exclude.NA=T)/1*100)

rifamean <- rifamean[-1,] # Get rid of unspecified row

rifamean <- rifamean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

rifagraphic <- ggplot(rifamean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Rifamycin")

# Sulphonamides
sulfamean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(sulfonamide, na.rm=T, exclude.NA=T)/1*100)

sulfamean <- sulfamean[-1,] # Get rid of unspecified row

sulfamean <- sulfamean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

sulfagraphic <- ggplot(sulfamean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Sulphonamides")

# Tetracyclines
tetmean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(tetracycline, na.rm=T, exclude.NA=T)/1*100)

tetmean <- tetmean[-1,] # Get rid of unspecified row

tetmean <- tetmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

tetgraphic <- ggplot(tetmean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Tetracyclines")

# Trimethoprim
trimean <- df %>%
  group_by(continent)%>%
  summarise(mean=mean(trimethoprim, na.rm=T, exclude.NA=T)/1*100)

trimean <- trimean[-1,] # Get rid of unspecified row

trimean <- trimean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

trigraphic <- ggplot(trimean, aes(x=continent, y=percentage, fill=continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Trimethoprim")

#Joining the graphs - excluded disinfectants as cannot fit
Continent_AMR_Class<-ggarrange(aminographic,betagraphic,colistingraphic,fosfographic,
                          fusidicgraphic,glycographic,macrographic,nitrographic,oxazgraphic,phegraphic,
                          pseudographic,quinographic,rifagraphic,sulfagraphic,tetgraphic,trigraphic)

#Adding the title and the axis
annotate_figure(Continent_AMR_Class, 
                #top=text_grob("AMR"), 
                bottom=text_grob("Continent"), 
                left = text_grob("Level of resistance %",rot = 90, vjust = 1))


