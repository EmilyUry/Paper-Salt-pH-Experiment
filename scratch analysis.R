

## Preliminary data poking


## Chapter 4


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")


data <- read.csv("chapter4_mater.csv", head = T)
head(data)

data$Site <- as.factor(data$Site)
data$pH_treat <- as.factor(data$pH_treat)
data$Sal_treat <- as.factor(data$Sal_treat)

data <- data[which(data$Sal_treat == 0),]

cols <- c("purple", "dodgerblue") 
pchs <- c(15, 16)
cols <- c("gray80", "gray50", "gray10")

boxplot(C_end ~ Site, data = data, col = c("purple", "dodgerblue"))
plot(DOC_mg_L_init~pH_init, data = data, pch = pchs[data$Site], col = cols[data$pH_treat] )
plot(DOC_mg_L_end~pH_end, data = data, pch = pchs[data$Site], col = cols[data$pH_treat] )

legend("bottomleft", c("Site 3", "Site 5"), pch = c(15,16))
plot(DOC_mg_L_end~pH_end, data = data, col = cols[data$Site])

abline(h = 11.2, col = "purple")
abline(h = 8.1, col = "dodgerblue")




data <- data[which(data$pH_n == "n"),]


Site3 <- data[which(data$Site == "3"),]
Site5 <- data[which(data$Site == "5"),]

pchs <- c(25, 21, 24)
cols <- c("gray100", "gray50", "gray10")

cols <- c("yellow", "orange", "red")

par(mfrow = c(1,2))
plot(Site3$DOC_mg_L_init, Site3$DOC_mg_L_end, cex =2, pch = 21,
     bg = cols[Site3$Sal_treat], ylim = c(0,70), xlim = c(0, 15), 
     main = "Acidic wetland soil \n pH ~4.5")
abline(a=0, b=1)
plot(Site5$DOC_mg_L_init, Site5$DOC_mg_L_end, cex =2, pch = 21,
     bg = cols[Site5$Sal_treat], ylim = c(0,70), xlim = c(0, 15),
     main = "Hydric wetland soil \n pH ~6")
abline(a=0, b=1)
legend("topright", c("0 ppt", "2.5 ppt" ,"10 ppt"), pt.bg = cols, pch =21, 
       pt.cex = 2, title = "Salinity Treatment" )



#### fractio of DOC rinsed out

data$C_frac_init <- data$DOC_mg_L_init/data$C_init
data$C_frac_end <- data$DOC_mg_L_end/data$C_init
Site3 <- data[which(data$Site == "3"),]
Site5 <- data[which(data$Site == "5"),]

par(mfrow = c(1,2))
plot(Site3$C_frac_init, Site3$C_frac_end, cex =2, pch = 21,
     bg = cols[Site3$Sal_treat], ylim = c(0,8), xlim = c(0, 2), 
     main = "Acidic wetland soil \n pH ~4.5")
abline(a=0, b=1)
plot(Site5$C_frac_init, Site5$C_frac_end, cex =2, pch = 21,
     bg = cols[Site5$Sal_treat], ylim = c(0,8), xlim = c(0, 2),
     main = "Hydric wetland soil \n pH ~6")
abline(a=0, b=1)
legend("topright", c("0 ppt", "2.5 ppt" ,"10 ppt"), pt.bg = cols, pch =21, 
       pt.cex = 2, title = "Salinity Treatment" )

