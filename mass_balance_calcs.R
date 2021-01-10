

### mass balance

library(ggplot2)


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)
data$fSite <- as.factor(data$Site)
data$Sal_treat <- as.factor(data$Sal_treat)

## conversions
# about 58 % of SOM is SOC


data$init_OC_mg_gs <- data$C_init * 0.58 / 100 * 1000

data$rinse_mg_gs <- data$DOC_mg_L_init * 0.01 ## 10 mL of rinse/gram soil

data$OC_post_rinse_mg_gs <- data$init_OC_mg_gs - data$rinse_mg_g

data$percent_rinse <- data$rinse_mg_g / data$init_OC_mg_gs * 100

par(mfrow = c(1,1), mar = c (4,5,2,2))

boxplot(percent_rinse~Site*Sal_treat, data = data, col = c("gray40","white"), xaxt ='n', xlab = " ", ylab = "Filtrate C as % of total SOC")
axis(1, at = c(1.5, 3.5, 5.5), c("0 ppt", "2.5 ppt", "10 ppt"))
legend("topright", c("Ponzer muck","Hyde loam"), pt.cex = 2, pt.bg = c("gray40", "white"), pch = 22)
text(c(1,2.2,3,4,5,6), 0.27, c("a", "a", "b", "b", "b", "b"))

data$response <- data$percent_rinse 
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
T1 <- data.frame(
  label = c("A", "A"),
  fSite   = c("3", "5"))
T2 <- data.frame(
  label = c("B", "B"),
  fSite   = c("3", "5"))
T3 <- data.frame(
  label = c("B", "B"),
  fSite   = c("3", "5"))

ggplot(data=data, aes(x=Sal_treat, y = response)) + 
  geom_boxplot() + 
  geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Filtrate C as % of total SOC") +   
  theme(legend.position = "none") + 
  ylim(0,0.35) +
  geom_text(data = T1, mapping = aes(x = 1, y = 0.34, label = label)) +
  geom_text(data = T2, mapping = aes(x = 2, y = 0.34, label = label)) +
  geom_text(data = T3, mapping = aes(x = 3, y = 0.34, label = label))










anova <- aov(percent_rinse~fSite*Sal_treat, data = data)
TukeyHSD((anova))
summary(anova)
TukeyHSD((anova), which = "Sal_treat")

Site3 <- data[which(data$Site == "3"),]
Site5 <- data[which(data$Site == "5"),]

#### ONE-WAY ANOVA
res.aov <- aov(percent_rinse ~ Sal_treat, data = Site3)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(percent_rinse ~ Sal_treat, data = Site5)
summary(res.aov)
TukeyHSD(res.aov)





data$response <- data$DOC_mg_L_init


### Stats
Site3 <- data[which(data$Site == "3"),]
Site5 <- data[which(data$Site == "5"),]

#### ONE-WAY ANOVA
res.aov <- aov(response ~ Sal_treat, data = Site3)
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- aov(response ~ Sal_treat, data = Site5)
summary(res.aov)
TukeyHSD(res.aov)


### TWO-WAY ANOVA
res.aov <- aov(response ~ Sal_treat*fSite, data = data)
summary(res.aov)
TukeyHSD(res.aov, which = "Sal_treat")



labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
T1 <- data.frame(
  label = c("A", "A"),
  fSite   = c("3", "5"))
T2 <- data.frame(
  label = c("B", "B"),
  fSite   = c("3", "5"))
T3 <- data.frame(
  label = c("B", "B"),
  fSite   = c("3", "5"))

ggplot(data=data, aes(x=Sal_treat, y = response)) + 
  geom_boxplot() + 
  geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
    ylab(expression(paste('DOC (mg · L'^-1, ')'))) +   
  theme(legend.position = "none") + 
  ylim(0,18) +
  geom_text(data = T1, mapping = aes(x = 1, y = 17.5, label = label)) +
  geom_text(data = T2, mapping = aes(x = 2, y = 17.5, label = label)) +
  geom_text(data = T3, mapping = aes(x = 3, y = 17.5, label = label))


Adata <- data[which(data$Sal_treat == "0"),]
Site3 <- Adata[which(Adata$Site == "3"),]
Site5 <- Adata[which(Adata$Site == "5"),]
#### ONE-WAY ANOVA
res.aov <- aov(response ~ pH_treat, data = Site3)
summary(res.aov)
TukeyHSD(res.aov)
res.aov <- aov(response ~ pH_treat, data = Site5)
summary(res.aov)
TukeyHSD(res.aov)
### TWO-WAY ANOVA
res.aov <- aov(response ~ pH_treat*fSite, data = Adata)
summary(res.aov)
TukeyHSD(res.aov, which = "pH_treat")



library(agricolae)
intx <- with(data, interaction(fSite, Sal_treat))
mod <- aov(percent_rinse ~ intx, data = data)
out <- HSD.test(mod,"intx", group=FALSE)
print(out$comparison)
out <- HSD.test(mod,"intx", group=TRUE)
out



plot(data$ugC.CO2_hr_gc, data$ugC.CO2_hr_gc_pr, pch = 19, col = c("gray40", "gray80", "black")[as.factor(data$Sal_treat)] )
abline(a=0, b=1.72)

