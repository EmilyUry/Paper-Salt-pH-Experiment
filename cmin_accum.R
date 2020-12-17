

library(ggplot2)


#### Cmin accumulation curves

## Chapter 4


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)

data$Site <- as.factor(data$Site)
data$pH_treat <- as.factor(data$pH_treat)
data$Sal_treat <- as.factor(data$Sal_treat)


info <- data[,c(1:5)]
x <- data[,27:35]

df <- cbind(info,x)

