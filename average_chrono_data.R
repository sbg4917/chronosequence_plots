library(xlsx)
library(plotrix)


setwd("C:/Users/Sarah/Google Drive/Hog Island/Hog Island/2017 Data/chronosequence analysis")

chrono_data_all <- read.csv("1990s and 2017 chronosequence data (no 1996).csv")

avg_chrono_data <- by(data = chrono_data_all, INDICES = list(chrono_data_all$Dataset, chrono_data_all$Zone.3..low.high., chrono_data_all$Age), FUN = mean)

avg_chrono_data <- aggregate(chrono_data_all, by = list(chrono_data_all$Dataset, chrono_data_all$Zone.3..low.high., chrono_data_all$Age), FUN = function(x) mean(x, na.rm = TRUE) )
SE_chrono_data <- aggregate(chrono_data_all, by =  list(chrono_data_all$Dataset, chrono_data_all$Zone.3..low.high., chrono_data_all$Age), FUN = function(x) std.error(x, na.rm = TRUE))
