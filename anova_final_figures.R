


## ANCOVAS and final figures

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)
data$fSite <- as.factor(data$Site)
data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)



library(ggplot2)
library(gridExtra)


data$response <- data$ugC.CO2_hr_gc ## cmin 3-day rate
data$response <- data$ugC.CO2_hr_gds ## cmin 3-day rater per gds
data$response <- data$T21_ug_CO2_gc  ## cmin 21 day cumulative (of days measured)

data$response <- data$C_end ## Carbon content end

data$response <- data$DOC_mg_L_end  ## doc end
data$response <- data$Phenol_mg_L_end  ## doc end
data$response <- data$SUVA254_end  ## doc end

data$phenol_perc_end <- data$Phenol_mg_L_end/data$DOC_mg_L_end
data$SUVA_perc_end <- data$SUVA254_end/data$DOC_mg_L_end
data$response <- data$phenol_perc_end
data$response <- data$SUVA_perc_end



### BASIC FIGURE STRUCTURE FOR BOX PLOT

par(mfrow = c(2,2))

labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
data$response <- data$response 
p1 <- ggplot(data=data, aes(x=Sal_treat, y = response, color = fSite)) + 
  geom_boxplot() + 
  geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab(expression(paste('C mineralization (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) + 
  theme(legend.position = "none")

p1

p2 <- ggplot(data=data, aes(x=Sal_treat, y = response, color = fSite)) + 
  geom_boxplot() + 
  geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab(expression(paste('C mineralization (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) + 
  theme(legend.position = "none")

grid.arrange(p1, p2, nrow = 2)




m1 <- lm(response~Sal_treat*fSite, data)
anova(m1)

Site3 <- data[which(data$Site == "3"),]
Site5 <- data[which(data$Site == "5"),]






fit <- aov(response~Sal_treat + fSite, data)
summary(fit)


fit <- aov(response~ fSite + Sal_treat, data)
summary(fit)


Anova(fit, type = "III") ### this is the same as summary.lm(fit)


#' The covariate, salt treatment, was significantly related to the response (cmin, 3day), F(3, 50) = 15.8



intx <- aov(response~Sal_treat*fSite, data)
summary(intx)

Anova(intx, type = "III") 


