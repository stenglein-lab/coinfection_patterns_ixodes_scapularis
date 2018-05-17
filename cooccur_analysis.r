#Co-occur Analysis
#R script
#Analysis using cooccur library
library(cooccur)
#This file needs to have the microbe names in the rows and tick #s as columns
#This file makes use of simple absent/present (1/0)
ticks <- read.csv(file.choose()) #cooccur_matrix.csv
View(ticks) #take a look at the file--I like to use this every time I make an edit to the file to make sure it didn't mess it up
#change the row names from numbers to microbes
rownames(ticks) <- c("South Bay virus",
                     "Suffolk virus",
                     "Blacklegged tick phlebovirus 1",
                     "Blacklegged tick phlebovirus 2",
                     "Blacklegged tick phlebovirus 3",
                     "Powassan virus",
                     "Ixodes scapularis associated virus 1",
                     "Ixodes scapularis associated virus 2",
                     "Borreliella burgdorferi",
                     "Borrelia mayonii",
                     "Borrelia miyamotoi",
                     "Babesia microti",
                     "Babesia odocoilei",
                     "Anaplasma phagocytophilum",
                     "Rickettsia",
                     "Ehrlichia",
                     "Wolbachia",
                     "Novel Worm")
ticks2 <- ticks[,2:113] #remove the first column--this is just the row names
ticks_cooccur <- cooccur(ticks2, type = "spp_site", thresh = TRUE, spp_names = TRUE) #running cooccur

#Look at the cooccur paper which explains each of these functions
#https://www.jstatsoft.org/article/view/v069c02
plot(ticks_cooccur)
summary(ticks_cooccur)
prob.table(ticks_cooccur)
pair.attributes(ticks_cooccur)
print(ticks_cooccur)
obs.v.exp(ticks_cooccur)
pair.profile(ticks_cooccur)
