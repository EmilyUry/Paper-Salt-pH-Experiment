library(ggplot2)
library(ggpubr)
library(multcomp)

## Chapter 4


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)

data$Site <- as.factor(data$Site)
data$pH_treat <- as.factor(data$pH_treat)
data$Sal_treat <- as.factor(data$Sal_treat)

##outlier removal
data[11,12] <- NA
data[11,13] <- NA


data$SUVA_perc_init <- data$SUVA254_init/data$DOC_mg_L_init*100
data$SUVA_perc_end <- data$SUVA254_end/data$DOC_mg_L_end*100

data$phenol_perc_init <- data$Phenol_mg_L_init/data$DOC_mg_L_init*100
data$phenol_perc_end <- data$Phenol_mg_L_end/data$DOC_mg_L_end*100



p <- ggplot(data, aes(x=Sal_treat, y=DOC_mg_L_end)) + 
        geom_boxplot() 
p + theme_bw() + facet_grid(pH_treat ~ Site)


intx <- with(data, interaction(pH_treat, Sal_treat, Site))
mod <- aov(DOC_mg_L_end ~ intx, data = data)
out <- HSD.test(mod,"intx", group=FALSE)
print(out$comparison)
out <- HSD.test(mod,"intx", group=TRUE)
out












sdata <- data[which(data$pH_n == 'n'),]
pdata <- data[which(data$Sal_treat == 0),]

par(mar = c(5,6,2,2))


### DOC initial rinse
boxplot(DOC_mg_L_init~Site*pH_treat, data, col = c("gray40", "white"),
        ylab = "DOC (mg/L)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,25))
legend("topleft", c("Acidic Site", "Cypress Swamp"), ncol = 2, 
       pt.bg = c("gray40", "white"), pch = 22, bty = "n", pt.cex = 1.5, cex = 0.8)
axis(1, c(1.5, 4.5, 7.5), c("5.5", "7.2", "8.8"))


## Phenol initial rinse
## outlier (11,12)
pdata[11,12] <- NA
pdata[11,13] <- NA
boxplot(Phenol_mg_L_init~Site*pH_treat, pdata, col = c("gray40", "white"),
        ylab = "Phenolic compounds (mg/L)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,4))
legend("topleft", c("Acidic Site", "Cypress Swamp"), ncol = 2, 
       pt.bg = c("gray40", "white"), pch = 22, bty = "n", pt.cex = 2)
axis(1, c(1.5, 4.5, 7.5), c("5.5", "7.2", "8.8"))


#### percent phenol
boxplot(phenol_perc_init~Site*pH_treat, pdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,25))
legend("topleft", c("Acidic Site", "Cypress Swamp"), ncol = 2, 
       pt.bg = c("gray40", "white"), pch = 22, bty = "n", pt.cex = 2)
axis(1, c(1.5, 4.5, 7.5), c("5.5", "7.2", "8.8"))

### absolute SUVA254
boxplot(SUVA254_init~Site*pH_treat, pdata, col = c("gray40", "white"),
        ylab = "SUVA254 (abs.)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,0.2))
legend("topleft", c("Acidic Site", "Cypress Swamp"), ncol = 2, 
       pt.bg = c("gray40", "white"), pch = 22, bty = "n", pt.cex = 2)
axis(1, c(1.5, 4.5, 7.5), c("5.5", "7.2", "8.8"))


#### percent SUVA254
boxplot(SUVA_perc_init~Site*pH_treat, pdata, col = c("gray40", "white"),
        ylab = "SUVA254 per mg/L DOC", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1))
legend("topleft", c("Acidic Site", "Cypress Swamp"), ncol = 2, 
       pt.bg = c("gray40", "white"), pch = 22, bty = "n", pt.cex = 2)
axis(1, c(1.5, 4.5, 7.5), c("5.5", "7.2", "8.8"))








### FIGURE 1A
par(mfrow = c(3,2), mar = c(0.5,2,0.5,1))
{boxplot(DOC_mg_L_init~Site*Sal_treat, sdata,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "DOC (mg/L)", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,20))
boxplot(DOC_mg_L_end~Site*Sal_treat, sdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "DOC (mg/L)", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,69))

boxplot(phenol_perc_init~Site*Sal_treat, sdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,28))
boxplot(phenol_perc_end~Site*Sal_treat, sdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,28))

boxplot(SUVA_perc_init~Site*Sal_treat, sdata,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "SUVA254 per mg/L DOC", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1.3))
boxplot(SUVA_perc_end~Site*Sal_treat, sdata,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "SUVA254 per mg/L DOC", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1.3))
}










### FIGURE 1B
par(mfrow = c(3,2), mar = c(0.5,2,0.5,1))
boxplot(DOC_mg_L_init~Site*pH_treat, pdata,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "DOC (mg/L)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,20))
boxplot(DOC_mg_L_end~Site*pH_treat, pdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "DOC (mg/L)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,75))

boxplot(phenol_perc_init~Site*pH_treat, pdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,23))
boxplot(phenol_perc_end~Site*pH_treat, pdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,23))

boxplot(SUVA_perc_init~Site*pH_treat, pdata,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "SUVA254 per mg/L DOC", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1.5))
boxplot(SUVA_perc_end~Site*pH_treat, pdata,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "SUVA254 per mg/L DOC", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1.5))



######### ANOVA

anova <- aov(DOC_mg_L_init ~ Site*Sal_treat, data = sdata)
TukeyHSD((anova))

##multicomp
#dht <- glht(anova, linfct = mcp(Sal_treat = "Tukey"))
#summary(dht)
library(agricolae)
intx <- with(sdata, interaction(Site, Sal_treat))
mod <- aov(phenol_perc_init ~ intx, data = sdata)
out <- HSD.test(mod,"intx", group=FALSE)
print(out$comparison)
out <- HSD.test(mod,"intx", group=TRUE)
out

intx <- with(pdata, interaction(Site, pH_treat))
mod <- aov(DOC_mg_L_init ~ intx, data = pdata)
out <- HSD.test(mod,"intx", group=FALSE)
print(out$comparison)
out <- HSD.test(mod,"intx", group=TRUE)
out







