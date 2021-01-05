


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

data$cmin <- data$ugC.CO2_hr_gc

data$SUVA_perc_end <- data$SUVA254_end/data$DOC_mg_L_end
data$phenol_perc_end <- data$Phenol_mg_L_end/data$DOC_mg_L_end



data$fSite <- as.factor(data$Site)
data$Sal_treat <- as.factor(data$Sal_treat)
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
ggplot(data=data, aes(x=sal_init, y = DOC_mg_L_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")

### ph 
simple <- lm(DOC_mg_L_end ~ pH_init, data = data)
summary(simple)
simple <- lm(DOC_mg_L_end ~ pH_init*fSite, data = data)
summary(simple)

ggplot(data=data, aes(x=pH_init, y = DOC_mg_L_end, color = fSite:Sal_treat)) + geom_point() + geom_smooth(method = "lm")

### Interaction

simple <- lm(DOC_mg_L_end ~ sal_init*pH_init*fSite, data = data)
summary(simple)

simple <- lm(DOC_mg_L_end ~ sal_init*pH_treat*fSite, data = data)
summary(simple)


##############################################
#
###### DOC fig for the pub
#
##############################################
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
ggplot(data=data, aes(x=sal_init, y = DOC_mg_L_end, color = pH_treat)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_grid(. ~ fSite, labeller = labeller(fSite = labs)) + 
  theme_bw() +
  xlab("Salinity (ppt)") +
  ylab("DOC (mg/L") + 
  labs(color = "pH")


simple <- lm(phenol_perc_end ~ sal_init, data = data)
summary(simple)
simple <- lm(phenol_perc_end ~ sal_init*fSite, data = data)
summary(simple)
ggplot(data=data, aes(x=sal_init, y = phenol_perc_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")






