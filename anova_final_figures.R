


## ANCOVAS and final figures

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)
data$fSite <- as.factor(data$Site)
data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)


Site3 <- data[which(data$Site == "3"),]
Site5 <- data[which(data$Site == "5"),]


library(ggplot2)
library(gridExtra)


# data$response <- data$ugC.CO2_hr_gc ## cmin 3-day rate
# data$response <- data$ugC.CO2_hr_gds ## cmin 3-day rater per gds
# data$response <- data$T21_ug_CO2_gc  ## cmin 21 day cumulative (of days measured)
# 
# data$response <- data$C_end ## Carbon content end
# 
# data$response <- data$DOC_mg_L_end  ## doc end
# data$response <- data$Phenol_mg_L_end  ## doc end
# data$response <- data$SUVA254_end  ## doc end
# 
# data$phenol_perc_end <- data$Phenol_mg_L_end/data$DOC_mg_L_end
# data$SUVA_perc_end <- data$SUVA254_end/data$DOC_mg_L_end
# data$response <- data$phenol_perc_end
# data$response <- data$SUVA_perc_end



### FIGURE 3 - Cmin(3day) 
{
data$response <- data$ugC.CO2_hr_gc ## cmin 3-day rate

labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
T1 <- data.frame(
  label = c("A", "A"),
  fSite   = c("3", "5"))
T2 <- data.frame(
  label = c("B", "B"),
  fSite   = c("3", "5"))
T3 <- data.frame(
  label = c("AB", "C"),
  fSite   = c("3", "5"))

ggplot(data=data, aes(x=Sal_treat, y = response)) + 
  geom_boxplot() + 
  geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) + 
  theme(legend.position = "none") + 
  ylim(0,11) +
  geom_text(data = T1, mapping = aes(x = 1, y = 10, label = label)) +
  geom_text(data = T2, mapping = aes(x = 2, y = 10, label = label)) +
  geom_text(data = T3, mapping = aes(x = 3, y = 10, label = label))

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
TukeyHSD(res.aov, which = "fSal_treat")
}


### FIGURE 4 (bottom) - Cmin(21 day total) 
{
  data$response <- data$T21_ug_CO2_gc ## cmin 21-day total
  
  labs <- c("Ponzer muck", "Hyde loam")
  names(labs) <- c("3", "5")
  T1 <- data.frame(
    label = c("A", "A"),
    fSite   = c("3", "5"))
  T2 <- data.frame(
    label = c("B", "B"),
    fSite   = c("3", "5"))
  T3 <- data.frame(
    label = c("B", "C"),
    fSite   = c("3", "5"))
  
  ggplot(data=data, aes(x=Sal_treat, y = response)) + 
    geom_boxplot() + 
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('C'[mineralization], '(', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +    
    theme(legend.position = "none") + 
    ylim(0,2500) +
    geom_text(data = T1, mapping = aes(x = 1, y = 2422, label = label)) +
    geom_text(data = T2, mapping = aes(x = 2, y = 2422, label = label)) +
    geom_text(data = T3, mapping = aes(x = 3, y = 2422, label = label))
  
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
  TukeyHSD(res.aov, which = "fSal_treat")
}


### Figure 5 C LOI
{
  data$response <- data$C_end ## Carbon content end
  
  labs <- c("Ponzer muck", "Hyde loam")
  names(labs) <- c("3", "5")
  T1 <- data.frame(
    label = c("A", "A"),
    fSite   = c("3", "5"))
  T2 <- data.frame(
    label = c("B", "B"),
    fSite   = c("3", "5"))
  T3 <- data.frame(
    label = c("C", "C"),
    fSite   = c("3", "5"))
  
  ggplot(data=data, aes(x=Sal_treat, y = response)) + 
    geom_boxplot() + 
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab("Soil organic matter (%)") +   
    theme(legend.position = "none") + 
    ylim(0, 14) +
    geom_text(data = T1, mapping = aes(x = 1, y = 13, label = label)) +
    geom_text(data = T2, mapping = aes(x = 2, y = 13, label = label)) +
    geom_text(data = T3, mapping = aes(x = 3, y = 13, label = label)) 
    # geom_hline(yintercept = 11.2, color = "1", size = 0.5, linetype = "dashed") +
    # geom_hline(yintercept = 8.1, color = "1", size = 0.5, linetype = "dashed") 
    
  
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
  TukeyHSD(res.aov, which = "fSal_treat")
}


### Figure 6 DOC, phenol and SUVA












