

## Chapter 4


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
names(data) <- c("Site", "Sal_treat", "pH_treat","pH_n","Sample_no","SM_init",
                 "C_init","sal_init","pH_init" ,"DOC_mg_L_init","TDN_mg_L_init",
                 "Phenol_mg_L_init", "SUVA254_init", "ugC.CO2_hr_gds","ugC.CO2_hr_gc",
                 "ugC_CH4_gds_hr", "ugC_CH4_gc_hr", "SM_end","C_end", "pH_end",
                 "sal_end","Phenol_mg_L_end", "SUVA254_end", "DOC_mg_L_end",
                 "TDN_mg_L_end", "Avg_cmin_21_ugC_hr_gc")
names(data) <- c("Site", "Sal_treat", "pH_treat","pH_n","Sample_no","SM_init",
                 "C_init","sal_init","pH_init" ,"DOC_mg_L_init","TDN_mg_L_init",
                 "Phenol_mg_L_init", "SUVA254_init", "C3_s","C3_c",
                 "CH4_s", "CH4_c", "SM_end","C_end", "pH_end",
                 "sal_end","Phenol_mg_L_end", "SUVA254_end", "DOC_mg_L_end",
                 "TDN_mg_L_end", "C21_c")


head(data)

data$Site <- as.factor(data$Site)
data$pH_treat <- as.factor(data$pH_treat)
data$Sal_treat <- as.factor(data$Sal_treat)

sdata <- data[which(data$pH_n == 'n'),]
pdata <- data[which(data$Sal_treat == 0),]



par(mfrow=c(2,2))

par(mfrow = c(3,1), mar = c(2,2,0.5,1))
boxplot(C3_s~Site*Sal_treat, sdata,
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "DOC (mg/L)", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,1.2))
axis(1, c(1.5, 4.5, 7.5), c("0", "2.5", "5"))
boxplot(C3_c~Site*Sal_treat, sdata, 
        col = c("rosybrown3", "white"), border = c("red4", "black"),
        ylab = "DOC (mg/L)", xlab = "Salinity Treatment", xaxt = 'n', 
        at = c(1,2,4,5,7,8),
        ylim = c(0,14))
axis(1, c(1.5, 4.5, 7.5), c("0", "2.5", "5"))

library(agricolae)
intx <- with(sdata, interaction(Site, Sal_treat))
mod <- aov(C3_s ~ intx, data = sdata)
out <- HSD.test(mod,"intx", group=FALSE)
print(out$comparison)
out <- HSD.test(mod,"intx", group=TRUE)
out


intx <- with(sdata, interaction(Site, Sal_treat))
mod <- aov(C3_c ~ intx, data = sdata)
out <- HSD.test(mod,"intx", group=FALSE)
print(out$comparison)
out <- HSD.test(mod,"intx", group=TRUE)
out





#C3_s, C3_c, CH4_s, CH4_c, C_end, C21_c
### FIGURE 2A
par(mfrow = c(3,1), mar = c(2,2,0.5,1))
boxplot(C3_s~Site*Sal_treat, sdata,
         col = c("rosybrown3", "white"), border = c("red4", "black"),
         ylab = "DOC (mg/L)", xlab = "Salinity Treatment", xaxt = 'n', 
         at = c(1,2,4,5,7,8),
         ylim = c(0,1.2))
axis(1, c(1.5, 4.5, 7.5), c("0", "2.5", "5"))
boxplot(C3_c~Site*Sal_treat, sdata, 
          col = c("rosybrown3", "white"), border = c("red4", "black"),
          ylab = "DOC (mg/L)", xlab = "Salinity Treatment", xaxt = 'n', 
          at = c(1,2,4,5,7,8),
          ylim = c(0,14))
axis(1, c(1.5, 4.5, 7.5), c("0", "2.5", "5"))

boxplot(C21_c~Site*Sal_treat, sdata,
          col = c("rosybrown3", "white"), border = c("red4", "black"),
          ylab = "SUVA254 per mg/L DOC", xlab = "Salinity Treatment", xaxt = 'n', 
          at = c(1,2,4,5,7,8),
          ylim = c(0,12))  
axis(1, c(1.5, 4.5, 7.5), c("0", "2.5", "5")) 
  
  
par(mfrow = c(3,1), mar = c(2,2,0.5,1))
boxplot(CH4_s~Site*Sal_treat, sdata, 
          col = c("rosybrown3", "white"), border = c("red4", "black"),
          ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "Salinity Treatment", xaxt = 'n', 
          at = c(1,2,4,5,7,8),
          ylim = c(0,0.5))
axis(1, c(1.5, 4.5, 7.5), c("0", "2.5", "5")) 
boxplot(CH4_c~Site*Sal_treat, sdata, 
          col = c("rosybrown3", "white"), border = c("red4", "black"),
          ylab = "Phenolic compounds\n(as percent of DOC)", xlab = "Salinity Treatment", xaxt = 'n', 
          at = c(1,2,4,5,7,8),
          ylim = c(0,6))
axis(1, c(1.5, 4.5, 7.5), c("0", "2.5", "5")) 
  

par(mfrow = c(3,1), mar = c(2,2,0.5,1))

boxplot(C_end~Site*Sal_treat, sdata,
          col = c("rosybrown3", "white"), border = c("red4", "black"),
          ylab = "SUVA254 per mg/L DOC", xlab = "Salinity Treatment", xaxt = 'n', 
          at = c(1,2,4,5,7,8),
          ylim = c(0,14))
  abline(h=11.2, col = "red4")
  abline(h = 8.1)
  boxplot(C21_c~Site*Sal_treat, sdata,
          col = c("rosybrown3", "white"), border = c("red4", "black"),
          ylab = "SUVA254 per mg/L DOC", xlab = "Salinity Treatment", xaxt = 'n', 
          at = c(1,2,4,5,7,8),
          ylim = c(0,12))











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







