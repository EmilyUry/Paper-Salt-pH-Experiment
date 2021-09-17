


###Final Figures 

## This script generates the final figures for the manuscript
## for the salt + pH lab experiment



library(ggplot2)
library(ggpubr)
library(gridExtra)
library(viridis)
library(tidyverse)
library(lubridate)



#setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")
setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Desktop/DukeBioDrop_backup/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)
data$fSite <- as.factor(data$Site)
data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)

Site3 <- data[which(data$Site == "3"),]
Site5 <- data[which(data$Site == "5"),]

### Fig1
## this figure is the methods diagram, made in ppt


### Fig2
{
data$cSite <- as.character(data$Site)

tiff(filename = "Fig2.tiff", height=2600, width=4800, units= "px", res=800, compression= "lzw")

ggline(data, x = "Sal_treat", y = "pH_init", shape = "pH_treat", linetype = "pH_treat", numeric.x.axis = TRUE,
       add = "mean_se", facet.by = "cSite", 
       ylab = "pH (filtrate)", xlab = "Salinity Treatment",
       panel.labs = list( cSite = c("Ponzer muck", "Hyde loam")), 
       ggtheme = theme_bw(), 
       legend = c(0.3,0.77), legend.title = "pH Treatment:")
dev.off()
}


### Fig2 REVISED
{
  data_summary <- function(data, varname, groupnames){
    require(plyr)
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        sd = sd(x[[col]], na.rm=TRUE),
        var = var(x[[col]], na.rm=TRUE))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }
  
  df2 <- data_summary(data, varname="pH_init", 
                      groupnames=c("pH_treat", "Sal_treat", "Site"))
  df3 <- data_summary(data, varname = "sal_init", groupnames=c("pH_treat", "Sal_treat", "Site"))
  df2$sal_init <- df3$sal_init
  df2$salty <- as.numeric(as.character(df2$Sal_treat))
  df2$se <- sqrt(df2$var/3)
  df2$salt_se <- sqrt(df3$var/3)
  
  labs <- c("Ponzer muck", "Hyde loam")
  names(labs) <- c("3", "5")
  line <- c(rep(1, 6), rep(3,6), rep(2, 6))

tiff(filename = "Fig2_revised.tiff", height=2600, width=4800, units= "px", res=800, compression= "lzw")

ggplot(df2, aes(x=sal_init, y=pH_init, shape = factor(pH_treat))) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=pH_init-se, ymax=pH_init+se), width = 0.3, size = 0.7) +
  #geom_segment(aes(x = sal_init-salt_se, xend=sal_init+salt_se, y = pH_init, yend = pH_init)) +
  geom_path(linetype = line) +
  theme_bw() +
  facet_grid(. ~ Site, labeller = labeller(Site = labs)) +
  xlab("Salinity Treatment (ppt)") +
  ylab("pH (filtrate)") +
  labs(shape = "pH Treatment") +
  theme(legend.position = c(.3, .75))

dev.off()
}

### Fig 3  - cmin 3 day
{
data$response <- data$ugC.CO2_hr_gc ## cmin 3-day rate

labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
T1 <- data.frame(
  label = c("A", "A"),
  fSite   = c("3", "5"))
T2 <- data.frame(
  label = c("A", "B"),
  fSite   = c("3", "5"))
T3 <- data.frame(
  label = c("A", "C"),
  fSite   = c("3", "5"))


tiff(filename = "Fig3.tiff", height=2400, width=3400, units= "px", res=800, compression= "lzw")

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

dev.off()

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
}

### Fig 4 - 21 day cmin accumulation, w Salinity labels on the plot and signficance numbers too

{
## Stats for 21-day accumulation
  
  data$response <- data$ugC.CO2_hr_gc ## cmin 3-day rate
  
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
  
## Figure for response curve
#data$response <- data$C_end ## Carbon content end

data4 <- read.csv("cmin_accum_long.csv", head=TRUE)
data4$Sal_treat <- as.factor(data4$Salt.treatment)
#data4$fSite <- as.character(data4$Site)
data4$fSite <- data4$Site

labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")

T1 <- data.frame(
  label = c("0 ppt (A)", "0 ppt (A)"),
  fSite   = c("3","5"),
  x = c(28, 28), 
  y = c(2000, 1600))
T2 <- data.frame(
  label = c("2.5 ppt (B)", "2.5 ppt (B)"),
  fSite   = c("3", "5"),
  x = c(29, 29), 
  y = c(1550, 1200))
T3 <- data.frame(
  label = c("10 ppt (B)", "10 ppt (C)"),
  fSite   = c("3", "5"),
  x = c(28.5, 28.5), 
  y = c(1420, 900))


tiff(filename = "Fig4.tiff", height=2400, width=4000, units= "px", res=800, compression= "lzw")

ggplot(data4, aes(Day, flux)) +
  scale_color_manual(values = c("black", "black", "black")) +
  stat_smooth(aes(linetype = Sal_treat, color = "black")) + 
  theme_bw() +
  facet_grid(.~fSite, labeller = labeller(fSite = labs)) +
  ylab(expression(paste('C'[mineralization], ' (', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +
  theme(legend.position = "none") +
  xlim(c(0,34))+
  geom_text(data = T1, mapping = aes(x=x, y=y, label = label))+
  geom_text(data = T2, mapping = aes(x=x, y=y, label = label))+
  geom_text(data = T3, mapping = aes(x=x, y=y, label = label))
  
dev.off()
}


## Figure 4 COLOR

tiff(filename = "Fig4_color.tiff", height=2400, width=4000, units= "px", res=800, compression= "lzw")

ggplot(data4, aes(Day, flux)) +
  scale_color_manual(values = c("#481567", "#33638D", "#3CBB75")) +
  scale_fill_manual(values = c("#6e5693", "#5f8ca1", "#bddc65")) +
  stat_smooth(aes(linetype = Sal_treat, color = Sal_treat, fill = Sal_treat)) + 
  theme_bw() +
  facet_grid(.~fSite, labeller = labeller(fSite = labs)) +
  ylab(expression(paste('C'[mineralization], ' (', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +
  theme(legend.position = "none") +
  xlim(c(0,34))+
  geom_text(data = T1, mapping = aes(x=x, y=y, label = label))+
  geom_text(data = T2, mapping = aes(x=x, y=y, label = label))+
  geom_text(data = T3, mapping = aes(x=x, y=y, label = label))

dev.off()


### Fig 5  -- DOC -- phenolics --- suva 
{
####  DOC

data$response <- data$DOC_mg_L_end  ## doc end
  
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
  geom_text(data = T3, mapping = aes(x = 3, y = 75, label = label)) +
  labs( tag = "A")

######## Phenol  
data$phenol_perc_end <- data$Phenol_mg_L_end/data$DOC_mg_L_end
data$response <- data$phenol_perc_end

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
  geom_text(data = T3, mapping = aes(x = 3, y = .19, label = label)) +
  labs( tag = "B")

##########  SUVA
data$SUVA_perc_end <- data$SUVA254_end/data$DOC_mg_L_end*100
data$response <- data$SUVA_perc_end

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
  geom_text(data = T3, mapping = aes(x = 3, y = 1.35, label = label)) +
  labs( tag = "C")


tiff(filename = "Fig5.tiff", height=5600, width=3400, units= "px", res=800, compression= "lzw")
grid.arrange(p1, p2, p3, ncol =1)
dev.off()

}

### Fig6 - black and white
{
data$fSal_treat <- as.factor(data$Sal_treat)
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")

tiff(filename = "Fig6bw.tiff", height=2600, width=3400, units= "px", res=800, compression= "lzw")

ggplot(data=data, aes(x=DOC_mg_L_end, y = Phenol_mg_L_end, shape = fSal_treat)) + 
  geom_point() + 
  scale_shape_manual(values = c(0,16, 3), name = " ", labels = c("0 ppt", "2.5 ppt", "10 ppt")) +
  geom_smooth(method = "lm", color = "black", size = 0.5) +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  theme(legend.position="top") +
  xlab(expression(paste('DOC (mg · L'^-1, ')'))) +
  ylab(expression(paste('Phenolics (mg · L' ^-1, ')'))) +
  theme(legend.title = element_blank(), legend.position = c(.9,.85), legend.direction = "vertical", 
        legend.background = element_blank()) 

dev.off()
}

## Fig6 - color
{
  
data$fSal_treat <- as.factor(data$Sal_treat)
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")

tiff(filename = "Fig6.tiff", height=2600, width=3400, units= "px", res=800, compression= "lzw")

ggplot(data=data, aes(x=DOC_mg_L_end, y = Phenol_mg_L_end, shape = fSal_treat, color = fSal_treat)) + 
  geom_point() + 
  scale_color_viridis(discrete = TRUE, begin = 0, end =0.85,  option ="D") +
  scale_shape_manual(values = c(0,16, 3), name = " ", labels = c("0 ppt", "2.5 ppt", "10 ppt")) +
  geom_smooth(aes(color = fSal_treat, fill = fSal_treat),method = "lm", color = "black", size = 0.5) +
  scale_fill_viridis(discrete = TRUE, begin = 0, end =0.95, option ="D") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  theme(legend.position="top") +
  xlab(expression(paste('DOC (mg · L'^-1, ')'))) +
  ylab(expression(paste('Phenolics (mg · L' ^-1, ')'))) +
  theme(legend.title = element_blank(), legend.position = c(.9,.85), legend.direction = "vertical", 
        legend.background = element_blank())  +
  guides(color = FALSE, fill = FALSE, shape = guide_legend(override.aes = list(color = c("#440154", "#287D8E", "#B8DE29"))))


dev.off()

}

#### Fig 7
{
data$response <- data$C_end ## Carbon content end
  
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
  label = c("C", "C"),
  fSite   = c("3", "5"))


T4 <- data.frame(x1 = c(0.5,0.5) , x2 = c(3.5,3.5), y1 = c(11.2, 8.1), fsite = c("3", "5"))


tiff(filename = "Fig7.tiff",  height=2400, width=3400, units= "px", res=800, compression= "lzw")

ggplot(data = data) + 
  geom_boxplot(aes(x=Sal_treat, y = response)) + 
  #geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Soil organic matter (%)") +   
  theme(legend.position = "none") + 
  ylim(0, 14) +
  geom_text(data = T1, mapping = aes(x = 1, y = 13, label = label)) +
  geom_text(data = T2, mapping = aes(x = 2, y = 13, label = label)) +
  geom_text(data = T3, mapping = aes(x = 3, y = 13, label = label)) +
  geom_segment(data = T4, aes(x = x1, xend = x2, 
                              y = y1, yend = y1), color = c("#BBBBBB99", "white", "white", "#BBBBBB99"), size = c(1, 1, 1, 1.5)) +  
  geom_segment(data = T4, aes(x = x1, xend = x2, 
                              y = y1, yend = y1), color = c("black", "white", "white", "black"), linetype = "dashed", size = 0.3) +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs))


dev.off()
}


############# Supplemental Figures

#### Fig S1
{

data$response <- data$sal_init ## salinity init
  
  T1 <- data.frame(
    label = c("A", "A"),
    fSite   = c("3", "5"))
  T2 <- data.frame(
    label = c("B", "B"),
    fSite   = c("3", "5"))
  T3 <- data.frame(
    label = c("C", "C"),
    fSite   = c("3", "5"))

IpH <- ggplot(data=data, aes(x=Sal_treat, y = response)) + 
  geom_boxplot() + 
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Salinity (ppt, initial filtrate)") + 
  theme(legend.position = "none") + 
  ylim(0, 13)+
  geom_text(data = T1, mapping = aes(x = 1, y = 11.5, label = label)) +
  geom_text(data = T2, mapping = aes(x = 2, y = 11.5, label = label)) +
  geom_text(data = T3, mapping = aes(x = 3, y = 11.5, label = label))



data$response <- data$sal_end ## salinity end

T1 <- data.frame(
  label = c("A", "A"),
  fSite   = c("3", "5"))
T2 <- data.frame(
  label = c("B", "B"),
  fSite   = c("3", "5"))
T3 <- data.frame(
  label = c("B", "B"),
  fSite   = c("3", "5"))

FpH <- ggplot(data=data, aes(x=Sal_treat, y = response)) + 
  geom_boxplot() + 
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Salinity (ppt, final extract)") + 
  theme(legend.position = "none") + 
  ylim(0,2)+
  geom_text(data = T1, mapping = aes(x = 1, y = 1.7, label = label)) +
  geom_text(data = T2, mapping = aes(x = 2, y = 1.7, label = label)) +
  geom_text(data = T3, mapping = aes(x = 3, y = 1.7, label = label))



tiff(filename = "FigS1.tiff", height=3200, width=3400, units= "px", res=800, compression= "lzw")
grid.arrange(IpH, FpH, nrow = 2)
dev.off()

}

## Fig S2
{
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")

data$response <- data$pH_init ## cmin 3-day rater per gds

T1 <- data.frame(
  label = c("A", "A"),
  fSite   = c("3", "5"))
T2 <- data.frame(
  label = c("B", "B"),
  fSite   = c("3", "5"))
T3 <- data.frame(
  label = c("B", "B"),
  fSite   = c("3", "5"))

IpH <- ggplot(data=data, aes(x=Sal_treat, y = response)) + 
  geom_boxplot() + 
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("pH (initial filtrate)") + 
  theme(legend.position = "none") + 
  ylim(4,7) +
  geom_text(data = T1, mapping = aes(x = 1, y = 6.8, label = label)) +
  geom_text(data = T2, mapping = aes(x = 2, y = 6.8, label = label)) +
  geom_text(data = T3, mapping = aes(x = 3, y = 6.8, label = label))


data$response <- data$pH_end ## cmin 3-day rater per gds
T1 <- data.frame(
  label = c("A", "A"),
  fSite   = c("3", "5"))
T2 <- data.frame(
  label = c("B", "B"),
  fSite   = c("3", "5"))
T3 <- data.frame(
  label = c("C", "C"),
  fSite   = c("3", "5"))

FpH <- ggplot(data=data, aes(x=Sal_treat, y = response)) + 
  geom_boxplot() + 
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("pH (final extract)") +
  theme(legend.position = "none") + 
  ylim(4,7) +
  geom_text(data = T1, mapping = aes(x = 1, y = 6.8, label = label)) +
  geom_text(data = T2, mapping = aes(x = 2, y = 6.8, label = label)) +
  geom_text(data = T3, mapping = aes(x = 3, y = 6.8, label = label))

tiff(filename = "FigS2.tiff", height=3200, width=3400, units= "px", res=800, compression= "lzw")
grid.arrange(IpH, FpH, nrow = 2)
dev.off()
}

## Fig S3
{
data$response <- data$pH_init ##
  
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")

IpH <- ggplot(data=data, aes(x=pH_treat, y = response)) + 
  geom_boxplot() + 
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("pH") +
  ylab("pH (initial filtrate)") + 
  theme(legend.position = "none") + 
  ylim(4,7)
IpH

data$response <- data$pH_end ## 

FpH <- ggplot(data=data, aes(x=pH_treat, y = response)) + 
  geom_boxplot() + 
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("pH") +
  ylab("pH (final extract)") +
  theme(legend.position = "none") + 
  ylim(4,7)

FpH
tiff(filename = "FigS3.tiff", height=3200, width=3400, units= "px", res=800, compression= "lzw")
grid.arrange(IpH, FpH, nrow = 2)
dev.off()
}


#### Fig S5
{
  data$response <- data$ugC.CO2_hr_gds ## cmin 3-day rate
  
  labs <- c("Ponzer muck", "Hyde loam")
  names(labs) <- c("3", "5")
  T1 <- data.frame(
    label = c("A", "A"),
    fSite   = c("3", "5"))
  T2 <- data.frame(
    label = c("A", "B"),
    fSite   = c("3", "5"))
  T3 <- data.frame(
    label = c("A", "C"),
    fSite   = c("3", "5"))
  
  
  tiff(filename = "FigS5.tiff", height=2400, width=3400, units= "px", res=800, compression= "lzw")
  
  ggplot(data=data, aes(x=Sal_treat, y = response)) + 
    geom_boxplot() + 
    geom_smooth(data=subset(data, fSite == "5"), method = "lm") +
    facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
    theme_bw() +
    xlab("Salinity (ppt)") +
    ylab(expression(paste('C'[mineralization],' (',mu, 'g C-CO'[2], ' hr'^-1, 'gds'^-1, ')'))) + 
    theme(legend.position = "none") + 
    ylim(0,1.25) +
    geom_text(data = T1, mapping = aes(x = 1, y = 1.2, label = label)) +
    geom_text(data = T2, mapping = aes(x = 2, y = 1.2, label = label)) +
    geom_text(data = T3, mapping = aes(x = 3, y = 1.2, label = label))
  
  dev.off()
}

#### Fig S6 Suva vs DOC
{
tiff(filename = "FigS6.tiff", height=2600, width=3400, units= "px", res=800, compression= "lzw") 

data$fSal_treat <- as.factor(data$Sal_treat)
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
ggplot(data=data, aes(x=DOC_mg_L_end, y = SUVA254_end, shape = fSal_treat, color = fSal_treat)) + 
  geom_point() + 
  scale_color_viridis(discrete = TRUE, begin = 0, end =0.85,  option ="D") +
  scale_shape_manual(values = c(0,16, 3), name = " ", labels = c("0 ppt", "2.5 ppt", "10 ppt")) +
  geom_smooth(aes(color = fSal_treat, fill = fSal_treat), method = "lm", color = "black", size = 0.5) +
  scale_fill_viridis(discrete = TRUE, begin = 0, end =0.95, option ="D") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  theme(legend.position="top") +
  xlab(expression(paste('DOC (mg · L'^-1, ')'))) +
  ylab("UV absorbance at 254 nm") +
  theme(legend.title = element_blank(), legend.position = c(.9,.85), legend.direction = "vertical", 
        legend.background = element_blank()) +
  guides(color = FALSE, fill = FALSE, shape = guide_legend(override.aes = list(color = c("#440154", "#287D8E", "#B8DE29"))))
dev.off()
}

