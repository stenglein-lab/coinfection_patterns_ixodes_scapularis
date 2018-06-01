#Corr.test from psych library returns pearson coefficients and p-vals

ticks <- read.csv(file.choose()) # heatmap_matrix.csv
ticks <- ticks[2:113,] #removes first row which is just empty in csv file
rownames(ticks) <- ticks[,1] #make the row names the tick #s
ticks <- ticks[,2:19] #remove the first column which are just tick #s
ticks[ticks == 0] <- NA

library(psych)
ticks.corr.test <- corr.test(ticks, use = "pairwise", method = "pearson", adjust = "bonferroni")
write.csv(ticks.corr.test$n,file="ntmp.csv")  ## sample sizes
write.csv(ticks.corr.test$t,file="ttmp.csv")  ## t statistics
write.csv(ticks.corr.test$p,file="ptmp.csv")  ## p-values
write.csv(ticks.corr.test$r, file = "rtmp.csv") ## coefficients
