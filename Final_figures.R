


###Final Figures

## This script generates the final figures for the manuscript
## for the salt + pH lab experiment



library(ggplot2)
library(gridExtra)
library(viridis)
library(tidyverse)
library(lubridate)



setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

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

tiff(filename = "Fig2.tiff", height=3200, width=4000, units= "px", res=800, compression= "lzw")

ggline(data, x = "Sal_treat", y = "pH_init", shape = "pH_treat", linetype = "pH_treat", numeric.x.axis = TRUE,
       add = "mean_se", facet.by = "cSite", 
       ylab = "pH (filtrate)", xlab = "Salinity Treatment",
       panel.labs = list( cSite = c("Ponzer muck", "Hyde loam")), 
       ggtheme = theme_bw(), 
       legend = c(0.2,0.8), legend.title = "pH Treatment:")
dev.off()
}
### Fig 3  - cmin 3 day



### Fig 4 - 21 day cmin accumulation, w Salinity labels on the plot and signficance numbers too

{

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


### Fig 5



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
  guides(color = FALSE, fill = FALSE)

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


tiff(filename = "Fig7.tiff", height=2800, width=2600, units= "px", res=800, compression= "lzw")

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
dev.off()
}




############# Supplemental Figures

#### Fig S1


#### Fig S4
{
tiff(filename = "FigS4.tiff", height=2600, width=3400, units= "px", res=800, compression= "lzw") 

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
  guides(color = FALSE, fill = FALSE)
dev.off()
}

