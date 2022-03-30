### Prevalence and abundance of identified microbes in Ixodes scapularis tick

#This script is an updated version to create Fig 2 from this paper:
#

#Created by: Shaun Cross

#Load in the packages
library(tidyverse)
library(ggrepel)

#Let's set the work dir
#for ease we will set the work dir to be the location this script is saved
#note: save this script in the same folder/dir as the data table
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Read in the data
prev.data <- read.csv('prev_abund_matrix.csv')
#what format is this file read in as? Table, data frame, list, etc.
class(prev.data)

#although this data gets read in as a dataframe, the format is not convenient to play around in the tidyverse
#Let's start playing around with the data to make it into tidy format:

#the original data frame has the tick numbers listed in the first column
#however, for this plot we don't need that information, so let's remove it
prev.data <- prev.data[,-1]

#Microbes
#Note: not only are we getting the data here, but it is the start of our new dataframe
prev.df.tidy <- as.data.frame(colnames(prev.data))
names(prev.df.tidy)[1] <- 'microbe'

#Mean mapping reads per million reads
#these are the numerical values listed in the data
prev.df.tidy$abund <- colMeans(prev.data)

#number of ticks positive for a microbe
#the way this works is we first make all values above 0 to 1
#we then will take the sum of each column which will be the prevalence (# of ticks)
prev.df.tidy$prev <- colSums(prev.data %>% mutate_if(is.numeric, ~1 * (. > 0)))

#Microbes that were studied more in depth in this study
#We chose a subset of microbes that were of interest to us
#We wanted to highlight the microbes we focused on

microbes_interest <- c("South.Bay.virus",
                       "Suffolk.virus",
                       "Blacklegged.tick.phlebovirus.1",
                       "Blacklegged.tick.phlebovirus.2",
                       "Blacklegged.tick.phlebovirus.3",
                       "Powassan.virus",
                       "Ixodes.scapularis.associated.virus.1",
                       "Ixodes.scapularis.associated.virus.2",
                       "Borreliella.burgdorferi",
                       "Borrelia.mayonii",
                       "Borrelia.miyamotoi",
                       "Babesia.microti",
                       "Babesia.odocoilei",
                       "Anaplasma.phagocytophilum",
                       "Rickettsia",
                       "Ehrlichia",
                       "Wolbachia",
                       "Novel.Worm")

prev.df.tidy$microbe_interest <- prev.df.tidy$microbe %in% microbes_interest

#The kingdom of the microbe (Eukaryote, Bacteria, Virus)
#This information can be identified by searching the literature

prev.df.tidy$microbe

#eukaryotic organisms
eukarya <- c("Babesia.microti",                   
             "Babesia.odocoilei",                   
             "Novel.Worm",
             "Cladosporium",
             "Stenotrophomonas",                    
             "Bradyrhizobium",                      
             "Zymoseptoria",                 
             "Delftia.acidovorans",                 
             "Aureobasidium",                       
             "Alternaria",             
             "Ralstonia.solanacearum",
             "Leptosphaeria",                       
             "Colletotrichum",                      
             "Aspergillus")
  
#bacterial organisms
bacteria <- c( "Borreliella.burgdorferi",             
               "Borrelia.mayonii",                 
               "Borrelia.miyamotoi",           
               "Anaplasma.phagocytophilum",           
               "Rickettsia",                          
               "Ehrlichia",                          
               "Wolbachia",                        
               "Escherichia",
               "Pseudomonas",                 
               "Bacillus.coagulans",
               "Mycobacterium.spp.")

#viral organisms
virus <- c("South.Bay.virus",        
           "Suffolk.virus",                    
           "Blacklegged.tick.phlebovirus.1",
           "Blacklegged.tick.phlebovirus.2",     
           "Blacklegged.tick.phlebovirus.3",     
           "Powassan.virus",     
           "Ixodes.scapularis.associated.virus.1",
           "Ixodes.scapularis.associated.virus.2")


prev.df.tidy$kingdom <- ifelse(prev.df.tidy$microbe %in% eukarya, 'Eukaryote', 
       ifelse(prev.df.tidy$microbe %in% bacteria, 'Bacteria', 'Virus'))

#Now let's plot this:
ggplot(prev.df.tidy, aes(x = prev, y = abund)) +
  geom_point(aes(shape = kingdom, color = microbe_interest),
             size = 3) +
  geom_label_repel(aes(label = microbe),
                   label.size = .1,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   size = 2) +
  scale_color_manual(values = c('Grey', 'Red')) +
  scale_y_log10() +
  theme_classic() +
  labs(title = "Prevalence and Abundance of Microbial Constituents",
       y = "Abundance (mean mapping reads per M)",
       x = "Prevalence (# of infected ticks out of 112)",
       shape = "Kingdom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = FALSE)
