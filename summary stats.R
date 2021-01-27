
library(plyr)
## summary

library(ggplot2)
library(gridExtra)



setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)


data$fSite <- as.factor(data$Site)
data$Sal_treat <- as.numeric(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)

data$cSite <- as.character(data$Site)



library(ggpubr)



p1 <- ggline(data, x = "Sal_treat", y = "pH_init", shape = "pH_treat", linetype = "pH_treat", numeric.x.axis = TRUE,
             add = "mean_se", facet.by = "cSite", 
             ylab = "pH (filtrate)", xlab = "Salinity Treatment",
             panel.labs = list( cSite = c("Ponzer muck", "Hyde loam")), 
             ggtheme = theme_bw(), 
             legend = c(0.2,0.7), legend.title = "pH Treatment:")
p1

# ggline(data, x = "sal_end", y = "pH_end", linetype = "pH_treat",
#        add = "mean_se", facet.by = "cSite", 
#        ylab = "pH (extract)", xlab = "Salinity Treatment",
#        panel.labs = list( cSite = c("Ponzer muck", "Hyde loam")), 
#        ggtheme = theme_bw(), 
#        legend = c(0.4,0.7), legend.title = "pH Treatment:")



group <- c(rep("A",6), rep("B", 6), rep("C", 6), rep("D", 6), rep("E", 6), 
           rep("F", 6), rep("G", 6), rep("H", 6), rep("I", 6))

data$group <- group


z <- ddply(data, .(Sal_treat, Site), summarise, 
           cmin = mean(ugC.CO2_hr_gc), 
           se.cmin = sqrt(var(ugC.CO2_hr_gc))/length(ugC.CO2_hr_gc))
names(z) <- c("group", "Site", "cmin_avg", "cmin_se")
#z$pHt <- c(rep(c("5.5", "5.5", "7.2", "7.2", "8.8", "8.8"), 3))



z



z <- ddply(data, .(Sal_treat, Site), summarise, 
           cmin = mean(T21_ug_CO2_gc), 
           se.cmin = sqrt(var(T21_ug_CO2_gc))/length(T21_ug_CO2_gc))
names(z) <- c("group", "Site", "cmin_avg", "cmin_se")
#z$pHt <- c(rep(c("5.5", "5.5", "7.2", "7.2", "8.8", "8.8"), 3))
z

z <- ddply(data, .(Sal_treat, Site), summarise, 
           cmin = mean(DOC_mg_L_end), 
           se.cmin = sqrt(var(DOC_mg_L_end))/length(DOC_mg_L_end))
z

##### C (LOI)

z <- ddply(data, .(Sal_treat, Site), summarise, 
           cmin = mean(C_end), 
           se.cmin = sqrt(var(C_end))/length(C_end))
z

#### initital filtrate pH

z <- ddply(data, .(Sal_treat, Site), summarise, 
           cmin = mean(pH_init), 
           se.cmin = sqrt(var(pH_init))/length(pH_init))
z
