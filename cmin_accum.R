

library(ggplot2)
library(tidyverse)
library(lubridate)

#### Cmin accumulation curves

## Chapter 4


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("cmin_accum_long.csv", head=TRUE)
data$Sal_treat <- as.factor(data$Salt.treatment)
data$fSite <- as.factor(data$Site)

### with Salinity labels on the plot

### FIGURE 4 (TOP)

labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")

## Left side

ggplot(data, aes(Day, flux, linetype = Sal_treat)) +
  stat_smooth() + 
  theme_bw() +
  facet_grid(.~fSite, labeller = labeller(fSite = labs)) +
  ylab(expression(paste('C'[mineralization], ' (', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +
  theme(legend.position = "none") +
  xlim(c(0,27)) +
  annotate(geom = "text", x = 24, y = 2000, label = "0 ppt") +
  annotate(geom = "text", x = 24.9, y = 1600, label = "2.5 ppt") +
  annotate(geom = "text", x = 24.5, y = 1400, label = "10 ppt")

## right side
ggplot(data, aes(Day, flux, linetype = Sal_treat)) +
  stat_smooth() + 
  theme_bw() +
  facet_grid(.~fSite, labeller = labeller(fSite = labs)) +
  ylab(expression(paste('C'[mineralization], ' (', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +
  theme(legend.position = "none") +
  xlim(c(0,27)) +
  annotate(geom = "text", x = 24, y = 1600, label = "0 ppt") +
  annotate(geom = "text", x = 24.9, y = 1200, label = "2.5 ppt") +
  annotate(geom = "text", x = 24.5, y = 900, label = "10 ppt")














#### this isn't working for some reason
T1 <- data.frame(
  y = c(2000, 1600),
  fSite = c("3", "5"))
T2 <- data.frame(
  y = c(1600, 1200),
  fSite   = c("3", "5"))
T3 <- data.frame(
  y = c(1400, 900),
  fSite   = c("3", "5"))


ggplot(data, aes(Day, flux, linetype = Sal_treat)) +
  stat_smooth() + 
  theme_bw() +
  facet_grid(.~fSite, labeller = labeller(fSite = labs)) +
  ylab(expression(paste('C'[mineralization], ' (', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +
  theme(legend.position = "none") +
  xlim(c(0,27)) 
  geom_text(data = T1, mapping = aes(x = 24, y = y, label = "0 ppt")) +
  geom_text(data = T2, mapping = aes(x = 24.9, y = y, label = "2.5 ppt")) +
  geom_text(data = T3, mapping = aes(x = 24.5, y = y, label = "10 ppt"))
  
  
  
  








### mumbo jumbo below this line
#######################################################






## right side
p <- ggplot(data, aes(Day, flux, linetype = Sal_treat))
p +  stat_smooth() + 
  theme_bw() + 
  facet_grid(.~Site, labeller = labeller(Site = labs)) +
  ylab(expression(paste('C'[mineralization], ' (', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +
  theme(legend.position = "none") + 
  scale_color_discrete(guide = FALSE) +
  #labs(linetype = "Salinity") +
  xlim(c(0,28)) +
  annotate(geom = "text", x = 24, y = 1600, label = "0 ppt") +
  annotate(geom = "text", x = 24.9, y = 1200, label = "2.5 ppt") +
  annotate(geom = "text", x = 24.5, y = 900, label = "10 ppt")


#scale_linetype_manual(values=c("solid", "solid", "solid", "dashed", "dashed", "dashed", "dotted", "dotted", "dotted"))









df <- read.csv("chapter4_mater.csv", head = T)
head(df)

df$Site <- as.factor(df$Site)
df$pH_treat <- as.factor(df$pH_treat)
df$Sal_treat <- as.factor(df$Sal_treat)

site3 <- df[which(df$Site == "3"),]
site5 <- df[which(df$Site == "5"),]


p <- ggplot(df, aes(x=Sal_treat, y=total)) + 
  geom_boxplot() 
p + theme_bw() + facet_grid(pH_treat ~ Site)



#### stats


anova <- aov(total ~ Sal_treat*pH_treat, data = site3)
TukeyHSD((anova))

##multicomp
#dht <- glht(anova, linfct = mcp(Sal_treat = "Tukey"))
#summary(dht)
library(agricolae)
intx <- with(site3, interaction(pH_treat, Sal_treat))
mod <- aov(total ~ intx, data = site3)
out <- HSD.test(mod,"intx", group=FALSE)
print(out$comparison)
out <- HSD.test(mod,"intx", group=TRUE)
out




