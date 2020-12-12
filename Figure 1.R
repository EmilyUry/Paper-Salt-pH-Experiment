

## Chapter 4


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)

data$Site <- as.factor(data$Site)
data$pH_treat <- as.factor(data$pH_treat)
data$Sal_treat <- as.factor(data$Sal_treat)

sdata <- data[which(data$pH_n == 'n'),]
data <- data[which(data$Sal_treat == 0),]

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
data[11,12] <- NA
data[11,13] <- NA
boxplot(Phenol_mg_L_init~Site*pH_treat, data, col = c("gray40", "white"),
        ylab = "Phenolic compounds (mg/L)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,4))
legend("topleft", c("Acidic Site", "Cypress Swamp"), ncol = 2, 
       pt.bg = c("gray40", "white"), pch = 22, bty = "n", pt.cex = 2)
axis(1, c(1.5, 4.5, 7.5), c("5.5", "7.2", "8.8"))


#### percent phenol
phenol_perc_init <- data$Phenol_mg_L_init/data$DOC_mg_L_init*100
phenol_perc_end <- data$Phenol_mg_L_end/data$DOC_mg_L_end*100

boxplot(phenol_perc_init~Site*pH_treat, data, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,25))
legend("topleft", c("Acidic Site", "Cypress Swamp"), ncol = 2, 
       pt.bg = c("gray40", "white"), pch = 22, bty = "n", pt.cex = 2)
axis(1, c(1.5, 4.5, 7.5), c("5.5", "7.2", "8.8"))

### absolute SUVA254
boxplot(SUVA254_init~Site*pH_treat, data, col = c("gray40", "white"),
        ylab = "SUVA254 (abs.)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,0.2))
legend("topleft", c("Acidic Site", "Cypress Swamp"), ncol = 2, 
       pt.bg = c("gray40", "white"), pch = 22, bty = "n", pt.cex = 2)
axis(1, c(1.5, 4.5, 7.5), c("5.5", "7.2", "8.8"))


#### percent SUVA254
SUVA_perc_init <- data$SUVA254_init/data$DOC_mg_L_init*100
SUVA_perc_end <- data$SUVA254_end/data$DOC_mg_L_end*100

boxplot(SUVA_perc_init~Site*pH_treat, data, col = c("gray40", "white"),
        ylab = "SUVA254 per mg/L DOC", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1))
legend("topleft", c("Acidic Site", "Cypress Swamp"), ncol = 2, 
       pt.bg = c("gray40", "white"), pch = 22, bty = "n", pt.cex = 2)
axis(1, c(1.5, 4.5, 7.5), c("5.5", "7.2", "8.8"))








### FIGURE 1A
par(mfrow = c(3,2), mar = c(0.5,2,0.5,1))
boxplot(DOC_mg_L_init~Site*Sal_treat, sdata,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "DOC (mg/L)", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,20))
boxplot(DOC_mg_L_end~Site*Sal_treat, sdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "DOC (mg/L)", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,75))

boxplot(phenol_perc_init~Site*Sal_treat, sdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,20))
boxplot(phenol_perc_end~Site*Sal_treat, sdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,20))

boxplot(SUVA_perc_init~Site*Sal_treat, sdata,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "SUVA254 per mg/L DOC", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1.5))
boxplot(SUVA_perc_end~Site*Sal_treat, sdata,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "SUVA254 per mg/L DOC", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1.5))











### FIGURE 1B
par(mfrow = c(3,2), mar = c(0.5,2,0.5,1))
boxplot(DOC_mg_L_init~Site*pH_treat, data,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "DOC (mg/L)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,20))
boxplot(DOC_mg_L_end~Site*pH_treat, data, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "DOC (mg/L)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,75))

boxplot(phenol_perc_init~Site*pH_treat, data, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,20))
boxplot(phenol_perc_end~Site*pH_treat, data, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,20))

boxplot(SUVA_perc_init~Site*pH_treat, data,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "SUVA254 per mg/L DOC", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1.5))
boxplot(SUVA_perc_end~Site*pH_treat, data,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "SUVA254 per mg/L DOC", xlab = "pH Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1.5))



######### MANOVA

#Test for normalcy in the dependent variables

library(mvnormtest)
data(EuStockMarkets)
C <- t(EuStockMarkets[15:29,1:4])
mshapiro.test(C)








