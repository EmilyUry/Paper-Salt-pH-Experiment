


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
TukeyHSD(res.aov, which = "Sal_treat")



### Alkalinity --- no sig.difference
Adata <- data[which(data$Sal_treat == "0"),]
ggplot(data=Adata, aes(x=pH_treat, y = response)) +
  geom_boxplot() +
  geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) +
  theme(legend.position = "none") +
  ylim(0,11) 
### Stats
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
  
  
  ### Alkalinity --- no sig.difference
  Adata <- data[which(data$Sal_treat == "0"),]
  ggplot(data=Adata, aes(x=pH_treat, y = response)) +
    geom_boxplot() +
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) +
    theme(legend.position = "none") +
    ylim(0,2500) 
  ### Stats
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
  
  
  
  
  
  ### Alkalinity --- no sig.difference
  Adata <- data[which(data$Sal_treat == "0"),]
  ggplot(data=Adata, aes(x=pH_treat, y = response)) +
    geom_boxplot() +
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) +
    theme(legend.position = "none") +
    ylim(0,12) 
  ### Stats
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
  
  
  
}


### Figure 6 DOC, phenol and SUVA

{
  data$response <- data$DOC_mg_L_end  ## doc end
  
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
  
p1 <-  ggplot(data=data, aes(x=Sal_treat, y = response)) + 
    geom_boxplot() + 
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('DOC (mg · L'^-1, ')'))) +   
    theme(legend.position = "none") + 
    ylim(0, 80) +
    geom_text(data = T1, mapping = aes(x = 1, y = 75, label = label)) +
    geom_text(data = T2, mapping = aes(x = 2, y = 75, label = label)) +
    geom_text(data = T3, mapping = aes(x = 3, y = 75, label = label)) 

 p1 
  
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
  
  
  
  
  
  ### Alkalinity --- no sig.difference
  Adata <- data[which(data$Sal_treat == "0"),]
  ggplot(data=Adata, aes(x=pH_treat, y = response)) +
    geom_boxplot() +
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) +
    theme(legend.position = "none") +
    ylim(0,80) 
  ### Stats
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
  
  
  

  ######## Phenol  
  data$phenol_perc_end <- data$Phenol_mg_L_end/data$DOC_mg_L_end
  data$response <- data$phenol_perc_end
  
  labs <- c("Ponzer muck", "Hyde loam")
  names(labs) <- c("3", "5")
  T1 <- data.frame(
    label = c("A", "A"),
    fSite   = c("3", "5"))
  T2 <- data.frame(
    label = c("B", "B"),
    fSite   = c("3", "5"))
  T3 <- data.frame(
    label = c("C", "B"),
    fSite   = c("3", "5"))
  
  p2 <-  ggplot(data=data, aes(x=Sal_treat, y = response)) + 
    geom_boxplot() + 
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('Phenolics (mg · mg DOC'^-1, ')'))) +   
    theme(legend.position = "none") + 
    ylim(0, 0.2) +
    geom_text(data = T1, mapping = aes(x = 1, y = .19, label = label)) +
    geom_text(data = T2, mapping = aes(x = 2, y = .19, label = label)) +
    geom_text(data = T3, mapping = aes(x = 3, y = .19, label = label)) 
  p2
  
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
  
  
  
  ### Alkalinity --- no sig.difference
  Adata <- data[which(data$Sal_treat == "0"),]
  ggplot(data=Adata, aes(x=pH_treat, y = response)) +
    geom_boxplot() +
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) +
    theme(legend.position = "none") +
    ylim(0,.2) 
  ### Stats
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
  
  
  
  
##########  SUVA
  data$SUVA_perc_end <- data$SUVA254_end/data$DOC_mg_L_end*100
  data$response <- data$SUVA_perc_end
  
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
  
  p3 <-  ggplot(data=data, aes(x=Sal_treat, y = response)) + 
    geom_boxplot() + 
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('SUVA'[254], ' (L· mg DOC'^-1, '· m'^-1, ')'))) +  
    theme(legend.position = "none") + 
    ylim(0, 1.4) +
    geom_text(data = T1, mapping = aes(x = 1, y = 1.35, label = label)) +
    geom_text(data = T2, mapping = aes(x = 2, y = 1.35, label = label)) +
    geom_text(data = T3, mapping = aes(x = 3, y = 1.35, label = label)) 
  p3
  
  
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
  
  
  
  
  
  ### Alkalinity --- no sig.difference
  Adata <- data[which(data$Sal_treat == "0"),]
  ggplot(data=Adata, aes(x=pH_treat, y = response)) +
    geom_boxplot() +
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) +
    theme(legend.position = "none") +
    ylim(0,1.5) 
  ### Stats
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
  
  
  
  
  grid.arrange(p1, p2, p3, nrow = 3)
  
}



#### Supplemental fig 2
{
  data$response <- data$ugC.CO2_hr_gds ## cmin 3-day rater per gds
  
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
    ylab(expression(paste('C'[mineralization],' (', mu, 'g C-CO'[2], ' hr'^-1, 'gds'^-1, ')'))) + 
    theme(legend.position = "none") + 
    ylim(0,1.2) +
    geom_text(data = T1, mapping = aes(x = 1, y = 1.15, label = label)) +
    geom_text(data = T2, mapping = aes(x = 2, y = 1.15, label = label)) +
    geom_text(data = T3, mapping = aes(x = 3, y = 1.15, label = label))
  
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
  
  
  
  
  ### Alkalinity --- no sig.difference
  Adata <- data[which(data$Sal_treat == "0"),]
  ggplot(data=Adata, aes(x=pH_treat, y = response)) +
    geom_boxplot() +
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) +
    theme(legend.position = "none") +
    ylim(0,1.2) 
  ### Stats
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
  
}


#### Supplemental fig 3
{
  
######## Phenol  
  data$response <- data$Phenol_mg_L_end  ## 

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

p4 <-  ggplot(data=data, aes(x=Sal_treat, y = response)) + 
  geom_boxplot() + 
  geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab(expression(paste('Phenolics (mg · L'^-1, ')'))) +   
  theme(legend.position = "none") + 
  ylim(0, 10) +
  geom_text(data = T1, mapping = aes(x = 1, y = 9.5, label = label)) +
  geom_text(data = T2, mapping = aes(x = 2, y = 9.5, label = label)) +
  geom_text(data = T3, mapping = aes(x = 3, y = 9.5, label = label)) 
p4

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






### Alkalinity --- no sig.difference
Adata <- data[which(data$Sal_treat == "0"),]
ggplot(data=Adata, aes(x=pH_treat, y = response)) +
  geom_boxplot() +
  geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) +
  theme(legend.position = "none") +
  ylim(0,10) 
### Stats
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



##########  SUVA
data$response <- data$SUVA254_end  ## doc end

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

p5 <-  ggplot(data=data, aes(x=Sal_treat, y = response)) + 
  geom_boxplot() + 
  geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab(expression(paste('SUVA '[254], ' (absorbance)'))) +  
  theme(legend.position = "none") + 
  ylim(0, 0.6) +
  geom_text(data = T1, mapping = aes(x = 1, y = .56, label = label)) +
  geom_text(data = T2, mapping = aes(x = 2, y = .56, label = label)) +
  geom_text(data = T3, mapping = aes(x = 3, y = .56, label = label)) 
p5


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





### Alkalinity --- no sig.difference
Adata <- data[which(data$Sal_treat == "0"),]
ggplot(data=Adata, aes(x=pH_treat, y = response)) +
  geom_boxplot() +
  geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'g C'^-1, ')'))) +
  theme(legend.position = "none") +
  ylim(0,1) 
### Stats
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





grid.arrange(p4, p5, nrow = 2)
}





