


### salinity and pH are correlated

library(ggplot2)
setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)


data$fSite <- as.factor(data$Site)
#data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)


fit <- lm(pH_init ~ pH_treat + fSite, data = data)
summary(fit)
fit <- lm(pH_init ~ Sal_treat + fSite, data = data)
summary(fit)

fit <- lm(pH_init ~ pH_treat + Sal_treat + fSite, data = data)
summary(fit)



labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
ggplot(data=data, aes(x=Sal_treat, y = pH_init, color = pH_treat)) + 
  stat_summary(fun=mean, geom="line", aes(group = pH_treat)) +
  geom_point(data = data, aes(x = sal_init, y = pH_init, color = pH_treat)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("pH") + 
  labs(color = "pH")


### same figure as above but black and white
ggplot(data=data, aes(x=Sal_treat, y = pH_init, shape = pH_treat)) + 
  stat_summary(fun=mean, geom="line", aes(group = pH_treat, linetype = pH_treat)) +
  geom_point(data = data, aes(x = sal_init, y = pH_init, shape = pH_treat)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("pH") + 
  labs(shape = "pH") +
  scale_shape_manual(values = c(0,16, 3)) 
# scale_linetype_manual(values = c("solid", "longdash","dotted"))

