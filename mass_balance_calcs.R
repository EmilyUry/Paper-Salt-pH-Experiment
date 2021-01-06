

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

boxplot(percent_rinse~Site*Sal_treat, data = data, col = c("gray40","white"), xaxt ='n', xlab = " ", ylab = "Organic C (%) lost in first rinse")
axis(1, at = c(1.5, 3.5, 5.5), c("0 ppt", "2.5 ppt", "10 ppt"))
legend("topright", c("Ponzer muck","Hyde loam"), pt.cex = 2, pt.bg = c("gray40", "white"), pch = 22)
text(c(1,2.2,3,4,5,6), 0.27, c("a", "a", "b", "b", "b", "b"))



anova <- aov(percent_rinse~fSite*Sal_treat, data = data)
TukeyHSD((anova))

library(agricolae)
intx <- with(data, interaction(fSite, Sal_treat))
mod <- aov(percent_rinse ~ intx, data = data)
out <- HSD.test(mod,"intx", group=FALSE)
print(out$comparison)
out <- HSD.test(mod,"intx", group=TRUE)
out



plot(data$ugC.CO2_hr_gc, data$ugC.CO2_hr_gc_pr, pch = 19, col = c("gray40", "gray80", "black")[as.factor(data$Sal_treat)] )
abline(a=0, b=1.72)

