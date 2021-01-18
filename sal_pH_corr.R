


### salinity and pH are correlated

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

library(plyr)
z <- ddply(data, .(group, Site), summarise, 
           pH_e = mean(pH_end), 
           sal_e = mean(sal_end),
           sepH = sqrt(var(pH_end))/length(pH_end),
           sesal= sqrt(var(sal_end))/length(sal_end))
names(z) <- c("group", "Site", "pH_end", "sal_end", "sepH", "sesal")
z$pHt <- c(rep(c("5.5", "5.5", "7.2", "7.2", "8.8", "8.8"), 3))

labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
p2 <- ggplot(data, aes(x = sal_end, y = pH_end)) +
  #geom_point(aes(color = group)) +
  geom_point(data = z, aes(shape = pHt)) +
  geom_line(data=z, aes(linetype = pHt)) +
  geom_errorbarh(data = z, aes(xmin = sal_end - sesal, xmax = sal_end + sesal, y = pH_end, height = 0.01)) +
  geom_errorbar(data = z, aes(ymin = pH_end - sepH, ymax = pH_end + sepH, x = sal_end)) +
  facet_grid(. ~ Site, labeller = labeller(Site = labs)) +
  xlab("Salinity(ppt)") +
  ylab("pH (extract)") +
  theme_bw() +
  theme(legend.position = "none")

grid.arrange(p1, p2, nrow = 2)





############### old stuff below this line #####################


labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
p1 <- ggplot(data=data, aes(x=Sal_treat, y = pH_init)) + 
  stat_summary(fun=mean, geom="line", aes(group = pH_treat, color = pH_treat)) +
  geom_point(data = data, aes(x = sal_init, y = pH_init, color = pH_treat, shape = pH_treat)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Initial Filtrate pH") + 
  scale_shape_manual(values = c(0,16, 3)) +
  theme(legend.title = element_blank(), legend.position = c(.25,.68), legend.direction = "vertical", 
        legend.background = element_blank()) 

p1

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
data$fpH_treat <- as.factor(data$pH_treat)

p2 <- ggplot(data=data, aes(x=fSal_treat, y = pH_end)) + 
  #stat_summary(fun=mean, geom="line", aes(group = pH_treat, linetype = pH_treat)) +
  stat_smooth(data=data, method = "lm", se = FALSE, size = 0.5, aes(x=sal_end, y = pH_end, group = pH_treat, color = pH_treat)) +
  geom_point(data = data, aes(x = sal_end, y = pH_end, color = pH_treat, shape = pH_treat)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Final Extract pH") + 
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
  geom_boxplot(data = data, aes(group = pH_treat:fSal_treat)) + 
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("pH Treatment") +
  ylab("Final pH") + 
  theme(legend.position = "none")


grid.arrange(p1, p3, p4, nrow = 3)



Site3 <- data[which(data$Site == "3"),]
Site5 <- data[which(data$Site == "5"),]

#### ONE-WAY ANOVA

res.aov <- aov(sal_end ~ fSal_treat, data = Site3)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(sal_end ~ fSal_treat, data = Site5)
summary(res.aov)
TukeyHSD(res.aov)


### TWO-WAY ANOVA

res.aov <- aov(sal_end ~ fSal_treat*fSite, data = data)
summary(res.aov)
TukeyHSD(res.aov, which = "fSal_treat")







#### ONE-WAY ANOVA

res.aov <- aov(pH_end ~ fSal_treat, data = Site3)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(pH_end ~ fSal_treat, data = Site5)
summary(res.aov)
TukeyHSD(res.aov)


### TWO-WAY ANOVA

res.aov <- aov(pH_end ~ fSal_treat*fSite, data = data)
summary(res.aov)










#### ONE-WAY ANOVA

res.aov <- aov(pH_end ~ fpH_treat, data = Site3)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(pH_end ~ fpH_treat, data = Site5)
summary(res.aov)
TukeyHSD(res.aov)


### TWO-WAY ANOVA

res.aov <- aov(pH_end ~ fpH_treat*fSite, data = data)
summary(res.aov)



#### interaction bewtween salt and pH treatment exist within each site, but 
#### I am not really sure what this means or how to explain it. 
res.aov <- aov(pH_end ~ fSal_treat*fpH_treat, data = Site3)
summary(res.aov)
res.aov <- aov(pH_end ~ fSal_treat*fpH_treat, data = Site5)
summary(res.aov)







#### ONE-WAY ANOVA

res.aov <- aov(pH_init ~ fSal_treat, data = Site3)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(pH_init ~ fSal_treat, data = Site5)
summary(res.aov)
TukeyHSD(res.aov)


### TWO-WAY ANOVA

res.aov <- aov(pH_init ~ fSal_treat*fSite, data = data)
summary(res.aov)
TukeyHSD(res.aov, which = "fSal_treat")
