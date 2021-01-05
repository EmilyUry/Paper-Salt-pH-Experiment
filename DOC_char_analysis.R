


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

data$SUVA_perc_end <- data$SUVA254_end/data$DOC_mg_L_end*100
data$phenol_perc_end <- data$Phenol_mg_L_end/data$DOC_mg_L_end*100

#### Site as Categorical and Salt treatment as categorical
class(data$Site)
class(data$Sal_treat)
class(data$pH_treat)

data$fSite <- as.factor(data$Site)
class(data$fSite)
levels(data$fSite)
data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)



#### Define Response of interest here
data$response <- data$DOC_mg_L_end  ## doc end

data$response <- data$phenol_perc_end
data$response <- data$SUVA_perc_end


################################################
## Treatment as continuous site as categorical

## simple linear
simple <- lm(phenol_perc_end ~ DOC_mg_L_end, data = data)
summary(simple)
par(mfrow = c(2, 2))
plot(simple)
d<-data
d$predicted <- predict(simple)
d$residuals <- residuals(simple)
library(dplyr) 
d %>% select(phenol_perc_end, predicted, residuals) %>% head()

library(ggplot2)


ggplot(d, aes(x = DOC_mg_L_end, y = phenol_perc_end)) + geom_point() 

ggplot(d, aes(x = DOC_mg_L_end, y = phenol_perc_end)) + geom_point() +
  geom_point(aes(y = predicted), shape = 1)


ggplot(d, aes(x = DOC_mg_L_end, y = phenol_perc_end)) + 
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) + 
  geom_segment(aes(xend = DOC_mg_L_end, yend = predicted), alpha = 0.5) +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, color = "lightgray" )

par(mfrow = c(1,1), mar = c (4,5,2,2))
plot(d$DOC_mg_L_init, d$phenol_perc_end)
abline(simple)




### Here we see that doc is obviously a very strong predictor of phenolic content. 97%
simple <- lm(Phenol_mg_L_end ~ DOC_mg_L_end, data = data)
summary(simple)
simple <- lm(Phenol_mg_L_end ~ DOC_mg_L_end*fSite, data = data)
summary(simple)
ggplot(data=data, aes(x=DOC_mg_L_end, y = Phenol_mg_L_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")



## is salinity explaining the residuals at all??

data$residuals <- residuals(simple)
resid <- lm(residuals ~ sal_init*fSite, data = data)
summary(resid) ## NO
ggplot(data=data, aes(x=sal_init, y = residuals, color = fSite)) + geom_point() + geom_smooth(method = "lm")





### what about the first rinse??  ## 57%
simple <- lm(Phenol_mg_L_init ~ DOC_mg_L_init*fSite, data = data)
summary(simple)
ggplot(data=data, aes(x=DOC_mg_L_init, y = Phenol_mg_L_init, color = fSite)) + geom_point() + geom_smooth(method = "lm")

## is salinity explaining the residuals at all??
data$residuals <- residuals(simple)
resid <- lm(residuals ~ sal_init*fSite, data = data)
summary(resid) ## NO
ggplot(data=data, aes(x=sal_init, y = residuals, color = fSite)) + geom_point() + geom_smooth(method = "lm")




### SUVA?
### Here we see that doc is obviously a very strong predictor of phenolic content. 98%
simple <- lm(SUVA254_end ~ DOC_mg_L_end, data = data)
summary(simple)
simple <- lm(SUVA254_end ~ DOC_mg_L_end*fSite, data = data)
summary(simple)
ggplot(data=data, aes(x=DOC_mg_L_end, y = SUVA254_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")

## is salinity explaining the residuals at all??
data$residuals <- residuals(simple)
resid <- lm(residuals ~ sal_init*fSite, data = data)
summary(resid) ## NO
ggplot(data=data, aes(x=sal_init, y = residuals, color = fSite)) + geom_point() + geom_smooth(method = "lm")

### what about the first rinse??  ## 58%
simple <- lm(SUVA254_init ~ DOC_mg_L_init*fSite, data = data)
summary(simple)
ggplot(data=data, aes(x=DOC_mg_L_init, y = SUVA254_init, color = fSite)) + geom_point() + geom_smooth(method = "lm")

## is salinity explaining the residuals at all??
data$residuals <- residuals(simple)
resid <- lm(residuals ~ sal_init*fSite, data = data)
summary(resid) ## NO
ggplot(data=data, aes(x=sal_init, y = residuals, color = fSite)) + geom_point() + geom_smooth(method = "lm")





### are SUVA and phenol coupled -- yes 86 %
simple <- lm(SUVA254_init ~ Phenol_mg_L_init*fSite, data = data)
summary(simple)
ggplot(data=data, aes(x=Phenol_mg_L_init, y = SUVA254_init, color = fSite)) + geom_point() + geom_smooth(method = "lm")

## is salinity explaining the residuals at all??
data$residuals <- residuals(simple)
resid <- lm(residuals ~ sal_init*fSite, data = data)
summary(resid) ## yes!! 46%
ggplot(data=data, aes(x=sal_init, y = residuals, color = fSite)) + geom_point() + geom_smooth(method = "lm")



### are SUVA and phenol coupled -- yes 99 %
simple <- lm(SUVA254_end ~ Phenol_mg_L_end*fSite, data = data)
summary(simple)
ggplot(data=data, aes(x=Phenol_mg_L_end, y = SUVA254_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")

## is salinity explaining the residuals at all??
data$residuals <- residuals(simple)
resid <- lm(residuals ~ sal_init*fSite, data = data)
summary(resid) ##NO
ggplot(data=data, aes(x=sal_init, y = residuals, color = fSite)) + geom_point() + geom_smooth(method = "lm")






### compare the first and second extract in terms of DOC, phenols and suva
library(emmeans)
library(ggplot2)


newd <- read.csv("DOC_char.csv", head = TRUE)
head(newd)
newd$Extract <- as.factor(newd$Extract)
newd$Site <- as.factor(newd$Site)
newd$Salinity <- as.factor(newd$Salinity)

##DOC/Phenol/Suva
newd$response <- newd$DOC_mg_L ## 
SiteSal <- lm(response ~ Extract*Salinity*Site, data = newd)
summary(SiteSal)
# plot
emmip(SiteSal, Salinity ~ Extract | Site, CIs = TRUE, nesting.order = TRUE, 
      xlab = "Extract", ylab = "DOC (mg/L)")

##DOC/Phenol/Suva
newd$response <- newd$Phenol_mg_L ## 
SiteSal <- lm(response ~ Extract*Salinity*Site, data = newd)
summary(SiteSal)
# plot
emmip(SiteSal, Salinity ~ Extract | Site, CIs = TRUE, nesting.order = TRUE, 
      xlab = "Extract", ylab = "Phenolics (mg/L)")

##DOC/Phenol/Suva
newd$response <- newd$SUVA254 ##  
SiteSal <- lm(response ~ Extract*Salinity*Site, data = newd)
summary(SiteSal)
# plot
emmip(SiteSal, Salinity ~ Extract | Site, CIs = TRUE, nesting.order = TRUE, 
      xlab = "Extract", ylab = "SUVA254")






# simple <- lm(phenol_perc_end ~ DOC_mg_L_end, data = data)
# summary(simple)
# simple <- lm(phenol_perc_end ~ DOC_mg_L_end*fSite, data = data)
# summary(simple)
# ggplot(data=data, aes(x=DOC_mg_L_end, y = phenol_perc_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")





PhenDOC <- lm(phenol_perc_end ~ DOC_mg_L_end*Sal_treat, data = data)
summary(PhenDOC)

plot(PhenDOC)

d$predicted <- predict(PhenDOC)
d$residuals <- residuals(PhenDOC)

ggplot(d, aes(x = DOC_mg_L_end, y = phenol_perc_end)) + 
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) + 
  geom_segment(aes(xend = DOC_mg_L_end, yend = predicted), alpha = 0.5) +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, color = "lightgray" )


par(mfrow = c(1,1), mar = c (4,5,2,2))
plot(d$sal_init, d$residuals, pch = 16, col = as.factor(d$Site))
residfit <- lm()



par(mfrow = c(1,1), mar = c (4,5,2,2))

ggplot(data=data, aes(x=DOC_mg_L_end, y = phenol_perc_end, shape = fSite, color = Sal_treat)) + geom_point() + geom_smooth(method = "lm")
ggplot(data=data, aes(x=DOC_mg_L_end, y = Phenol_mg_L_end, shape = fSite, color = Sal_treat)) + geom_point() + geom_smooth(method = "lm")

ggplot(data=data, aes(x=sal_init, y = Phenol_mg_L_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")
ggplot(data=data, aes(x=sal_init, y = phenol_perc_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")


PhenDOC <- lm(phenol_perc_end ~ sal_init*fSite, data = data)
summary(PhenDOC)
ggplot(data=data, aes(x=sal_init, y = phenol_perc_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")

PhenDOC <- lm(Phenol_mg_L_end ~ sal_init*fSite, data = data)
summary(PhenDOC)
ggplot(data=data, aes(x=sal_init, y = Phenol_mg_L_end, color = fSite)) + geom_point() + geom_smooth(method = "lm")
