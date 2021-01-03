##

## lm scratch


library(emmeans)
library(ggplot2)


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)

data$cmin <- data$ugC.CO2_hr_gc

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
data$response <- data$ugC.CO2_hr_gc ## cmin 3-day rate
data$response <- data$DOC_mg_L_init  ## doc init
data$response <- data$DOC_mg_L_end  ## doc end
data$response <- data$ugC.CO2_hr_gds ## cmin 3-day rater per gds
data$response <- data$T21_ug_CO2_gc  ## cmin 21 day cumulative (of days measured)
data$response <- data$C_init ## Carbon content at start (should have no effect of treatments)
data$response <- data$C_end ## Carbon content end


SiteSal <- lm(response ~ fSite*Sal_treat, data = data)

summary(SiteSal)

# plot
emmip(SiteSal, Sal_treat ~ fSite, CIs = TRUE)


SitepH <- lm(response ~ fSite*pH_treat, data = data)
summary(SitepH)

#plot
emmip(SitepH, pH_treat ~ fSite, CIs = TRUE)


################################################
## Treatment as continuous site as categorical

SalSite <- lm(response ~ sal_init*fSite, data = data)
summary(SalSite)


p <- ggplot(data=data, aes(x=sal_init, y = response, color =fSite)) + geom_point() + geom_smooth(method = "lm")
p

### model w simulated data
# (mylist <- list(sal_init=seq(0,10,by=1), fSite=c("3","5")))
# contcatdat <- emmip(SalSite, fSite~sal_init, at=mylist, CIs=TRUE, plotit=FALSE)
# (p <- ggplot(data=contcatdat, aes(x=sal_init, y=yvar, color=fSite)) + geom_line())
# (p1 <- p + geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=fSite), alpha=0.4))
# (p2 <- p1  + labs(x="Salinity", y="Response", color="Site", fill="Site"))

pHSite <- lm(response ~ pH_init*fSite, data = data)
summary(pHSite)

p <- ggplot(data=data, aes(x=pH_init, y = response, color =fSite, shape = pH_treat)) + geom_point() + geom_smooth(method = "lm")
p

