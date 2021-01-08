


### salinity and pH are correlated

library(ggplot2)
setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)


data$fSite <- as.factor(data$Site)
#data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)



labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
p1 <- ggplot(data=data, aes(x=Sal_treat, y = pH_init)) + 
  stat_summary(fun=mean, geom="line", aes(group = pH_treat, color = pH_treat)) +
  geom_point(data = data, aes(x = sal_init, y = pH_init, color = pH_treat, shape = pH_treat)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("pH") + 
  scale_shape_manual(values = c(0,16, 3)) +
  theme(legend.title = element_blank(), legend.position = c(.25,.68), legend.direction = "vertical", 
        legend.background = element_blank()) 



### same figure as above but black and white
ggplot(data=data, aes(x=Sal_treat, y = pH_init)) + 
  stat_summary(fun=mean, geom="line", aes(group = pH_treat, linetype = pH_treat)) +
  geom_point(data = data, aes(x = sal_init, y = pH_init, shape = pH_treat)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("pH") + 
  labs(shape = "pH:") + 
  labs(linetype = " ")+
  scale_shape_manual(values = c(0,16, 3)) +
  theme(legend.position = "top")
# scale_linetype_manual(values = c("solid", "longdash","dotted"))




#### final extract
data$fSal_treat <- as.factor(data$Sal_treat)


p2 <- ggplot(data=data, aes(x=fSal_treat, y = pH_end)) + 
  #stat_summary(fun=mean, geom="line", aes(group = pH_treat, linetype = pH_treat)) +
  stat_smooth(data=data, method = "lm", se = FALSE, size = 0.5, aes(x=sal_end, y = pH_end, group = pH_treat, color = pH_treat)) +
  geom_point(data = data, aes(x = sal_end, y = pH_end, color = pH_treat, shape = pH_treat)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("pH") + 
  scale_shape_manual(values = c(0,16, 3)) +
  theme(legend.position = "none")
 


grid.arrange(p1, p2, nrow = 2)







p3 <- ggplot(data=data, aes(x=fSal_treat, y = sal_end)) + 
  geom_boxplot(data = data, aes(group = fSal_treat)) + 
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity Treatment") +
  ylab("Final Salinity (ppt)") + 
  theme(legend.position = "none")


p4 <- ggplot(data=data, aes(x=pH_treat, y = pH_end)) + 
  geom_boxplot(data = data, aes(group = pH_treat)) + 
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity Treatment") +
  ylab("Final pH") + 
  theme(legend.position = "none")


grid.arrange(p1, p3, p4, nrow = 3)




