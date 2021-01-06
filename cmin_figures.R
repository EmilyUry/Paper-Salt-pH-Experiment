



#### Cmin figures



setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)




data$fSite <- as.factor(data$Site)
data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)




#### Define Response of interest here
data$response <- data$ugC.CO2_hr_gc ## cmin 3-day rate
data$response <- data$ugC.CO2_hr_gc_pr ## cmin 3-day rate on a per carbon post rinse

data$response <- data$ugC.CO2_hr_gds ## cmin 3-day rater per gds
data$response <- data$T21_ug_CO2_gc  ## cmin 21 day cumulative (of days measured)
data$response <- data$C_init ## Carbon content at start (should have no effect of treatments)
data$response <- data$C_end ## Carbon content end



# #### Define Response of interest here
# data$response <- data$DOC_mg_L_end  ## doc end
# 
# data$response <- data$phenol_perc_end
# data$response <- data$SUVA_perc_end



### DOC is predicted by salinity (Final extract)
simple <- lm(DOC_mg_L_end ~ fSite, data = data)
summary(simple)
simple <- lm(DOC_mg_L_end ~ sal_init, data = data)
summary(simple)
simple <- lm(DOC_mg_L_end ~ sal_init*fSite, data = data)
summary(simple)
ggplot(data=data, aes(x=sal_init, y = DOC_mg_L_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")

### ph 
simple <- lm(DOC_mg_L_end ~ pH_init, data = data)
summary(simple)
simple <- lm(DOC_mg_L_end ~ pH_init*fSite, data = data)
summary(simple)

ggplot(data=data, aes(x=pH_init, y = DOC_mg_L_end, color = fSite:Sal_treat)) + geom_point() + geom_smooth(method = "lm")

### Interaction

intx <- lm(DOC_mg_L_end ~ sal_init*pH_init*fSite, data = data)
summary(intx)

simple <- lm(DOC_mg_L_end ~ sal_init + pH_treat + fSite, data = data)
summary(simple)


anova(simple, intx)   #are the models significantly different.

##############################################
#
###### Cmin fig for the pub
#
##############################################
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
ggplot(data=data, aes(x=sal_init, y = response, color = fSite)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Cmin (3-day)") + 
  labs(color = "pH")+
  theme(legend.position = "none")


data$response <- data$ugC.CO2_hr_gds
ggplot(data=data, aes(x=sal_init, y = response, color = fSite)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Cmin (per gram soil)") + 
  labs(color = "") +
  theme(legend.position = "none")


#### 21 day cum
data$response <- data$T21_ug_CO2_gc 
ggplot(data=data, aes(x=sal_init, y = response, color = fSite)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Cmin 21-day total") + 
  labs(color = "") +
  theme(legend.position = "none")

