########## Sarah Goldsmith 2/28/2019 ############################################
#################################################################################
# code to take a random subset of 10 height measurements from 1995 plant survey #
#################################################################################


library(xlsx)

setwd("C:/Users/Sarah/Google Drive/Hog Island/Hog Island/2017 Data/chronosequence analysis")

walsh_heights <- read.xlsx("1990s new biomass regression.xlsx", 6, row.names = TRUE)
walsh_density <- walsh_heights[1]
walsh_heights <- walsh_heights[-1] #remove density values
walsh_subset <- data.frame(t(apply(walsh_heights, 1, function(d) sample(na.omit(d), 10))))
#walsh_subset$density <- walsh_density[1]



tyler_heights <- read.xlsx("1990s new biomass regression.xlsx", 10, row.names = TRUE)
tyler_density <- tyler_heights[1]
tyler_heights <- tyler_heights[-1]
tyler_subset <- data.frame(t(apply(tyler_heights, 1, function(d) sample(na.omit(d), 10))))
#tyler_subset$density <- tyler_density[1]



height_subset <- rbind(walsh_subset, tyler_subset)
write.csv(height_subset, file = "biomass height subset.csv")

