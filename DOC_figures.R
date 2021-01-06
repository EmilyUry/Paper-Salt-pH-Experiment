


library(emmeans)
library(ggplot2)

### DOC characteristics analysis

### Salinity greatly reduces the amount of DOC in the final extract, 
## salinity is also reducing the proportion of phenolic and UV active molecules in that DOC
## but is this just because these two properties are correlated, or does the treatment explain some
## of the residual variance on this relationship

## use a continuous by categorical linear model as we did in the lm_scratch.R file

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)


data$SUVA_perc_end <- data$SUVA254_end/data$DOC_mg_L_end
data$phenol_perc_end <- data$Phenol_mg_L_end/data$DOC_mg_L_end



data$fSite <- as.factor(data$Site)
#data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)



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
#ggplot(data=data, aes(x=sal_init, y = DOC_mg_L_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")

### ph 
simple <- lm(DOC_mg_L_end ~ pH_init, data = data)
summary(simple)
simple <- lm(DOC_mg_L_end ~ pH_init*fSite, data = data)
summary(simple)

#ggplot(data=data, aes(x=pH_init, y = DOC_mg_L_end, color = fSite:Sal_treat)) + geom_point() + geom_smooth(method = "lm")

### Interaction

intx <- lm(DOC_mg_L_end ~ sal_init*pH_init*fSite, data = data)
summary(intx)

simple <- lm(DOC_mg_L_end ~ sal_init + pH_treat + fSite, data = data)
summary(simple)


anova(simple, intx)   #are the models significantly different.

##############################################
#
###### DOC fig for the pub
#
##############################################
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")

ggplot(data=data, aes(x=Sal_treat, y = DOC_mg_L_end, color = fSite)) + 
  geom_point(data = data, aes(x = sal_init, y = DOC_mg_L_end, color = fSite)) + 
  #geom_line() +
  stat_summary(fun=mean, geom="line", aes(group = 1)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("DOC (mg/L)") + 
  labs(color = "pH")+
  theme(legend.position = "none")



#### phenol per doc
ggplot(data=data, aes(x=Sal_treat, y = phenol_perc_end, color = fSite)) + 
  geom_point(data = data, aes(x = sal_init, y = phenol_perc_end, color = fSite)) + 
  #geom_line() +
  stat_summary(fun=mean, geom="line", aes(group = 1)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Phenolics (mg/ mg DOC)") + 
  labs(color = "") +
  theme(legend.position = "none")

#### SUVA per doc
ggplot(data=data, aes(x=Sal_treat, y = SUVA_perc_end, color = fSite)) + 
  geom_point(data = data, aes(x = sal_init, y = SUVA_perc_end, color = fSite)) + 
  #geom_line() +
  stat_summary(fun=mean, geom="line", aes(group = 1)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("SUVA254 L / mg DOC /cm ") + 
  labs(color = "") +
  theme(legend.position = "none")



########################################

#           Supplemental figs

########################################




#### phenol per doc
ggplot(data=data, aes(x=Sal_treat, y = Phenol_mg_L_end, color = fSite)) + 
  geom_point(data = data, aes(x = sal_init, y = Phenol_mg_L_end, color = fSite)) + 
  #geom_line() +
  stat_summary(fun=mean, geom="line", aes(group = 1)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("Phenolics (mg/L)") + 
  labs(color = "") +
  theme(legend.position = "none")

#### SUVA per doc
ggplot(data=data, aes(x=Sal_treat, y = SUVA254_end, color = fSite)) + 
  geom_point(data = data, aes(x = sal_init, y = SUVA254_end, color = fSite)) + 
  #geom_line() +
  stat_summary(fun=mean, geom="line", aes(group = 1)) +
  #geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("SUVA254 (absorbance)") + 
  labs(color = "") +
  theme(legend.position = "none")









##############################################
#
###### SUVA and Phen are correlated w DOC
#
##############################################
data$fSal_treat <- as.factor(data$Sal_treat)
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
ggplot(data=data, aes(x=DOC_mg_L_end, y = Phenol_mg_L_end, color = fSal_treat, shape = fSite)) + 
  geom_point() + 
  scale_shape_manual(values = c(0,16), name = "", labels = c("Ponzer muck", "Hdye loam")) +
  geom_smooth(method = "lm") +
 # facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  theme(legend.position="top") +
  xlab("DOC (mg/L)") +
  ylab("Phenolics (mg/L)") + 
  scale_color_discrete(name = "", labels = c("0 ppt", "2.5 ppt", "10 ppt"))
  #scale_shape_discrete(name = "", labels = c("Ponzer muck", "Hdye loam"))



fit <- lm(Phenol_mg_L_end ~ DOC_mg_L_end*fSite, data = data)
summary(fit)


ggplot(data=data, aes(x=DOC_mg_L_end, y = SUVA254_end, color = Sal_treat, shape = fSite)) + 
  geom_point() + 
  scale_shape_manual(values = c(0,16), name = "", labels = c("Ponzer muck", "Hdye loam")) +
  geom_smooth(method = "lm") +
  # facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  theme(legend.position="top") +
  xlab("DOC (mg/L)") +
  ylab("UV absorbance at 254 nm") + 
  scale_color_discrete(name = "", labels = c("0 ppt", "2.5 ppt", "10 ppt"))
#scale_shape_discrete(name = "", labels = c("Ponzer muck", "Hdye loam"))




ggplot(data=data, aes(x=DOC_mg_L_end, y = SUVA254_end, color = fSite)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  # facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  theme(legend.position="top") +
  xlab("DOC (mg/L)") +
  ylab("UV absorbance at 254 nm") + 
  scale_color_discrete(name = "", labels = c("Ponzer muck", "Hdye loam"))

fit <- lm(SUVA254_end ~ DOC_mg_L_end*fSite, data = data)
summary(fit)





##################### regression analysis for pub table
# 
# 
# simple <- lm(phenol_perc_end ~ sal_init, data = data)
# summary(simple)
# simple <- lm(phenol_perc_end ~ sal_init*fSite, data = data)
# summary(simple)
# ggplot(data=data, aes(x=sal_init, y = phenol_perc_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")
# 
# 
# 
# 
# 
# simple <- lm(Phenol_mg_L_end ~ SUVA254_end, data = data)
# summary(simple)
# intx <- lm(Phenol_mg_L_end ~ SUVA254_end + fSite, data = data)
# summary(intx)
# anova(simple, intx)
# 
# 
# ggplot(data=data, aes(x=Phenol_mg_L_end, y = SUVA254_end, color = Sal_treat)) + geom_point() + geom_smooth(method = "lm") +
#   facet_grid(. ~ fSite, labeller = labeller(fSite = labs))
# intx <- lm(Phenol_mg_L_end ~ SUVA254_end  + Sal_treat, data = data)
# summary(intx)
# anova(simple, intx)
# 
# 
# 
# 
# ggplot(data=data, aes(x=phenol_perc_end, y = SUVA_perc_end, color = Sal_treat)) + geom_point() + geom_smooth(method = "lm") +
#   facet_grid(. ~ fSite, labeller = labeller(fSite = labs))



