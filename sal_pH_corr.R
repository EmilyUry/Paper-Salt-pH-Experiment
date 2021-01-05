


### salinity and pH are correlated

library(ggplot2)
setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)


data$fSite <- as.factor(data$Site)
data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)


fit <- lm(pH_init ~ pH_treat + fSite, data = data)
summary(fit)
fit <- lm(pH_init ~ Sal_treat + fSite, data = data)
summary(fit)

fit <- lm(pH_init ~ pH_treat + Sal_treat + fSite, data = data)
summary(fit)



labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
ggplot(data=data, aes(x=sal_init, y = pH_init, color = pH_treat)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs))
