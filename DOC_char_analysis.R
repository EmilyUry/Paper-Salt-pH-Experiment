


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



PhenDOC <- lm(phenol_perc_end ~ DOC_mg_L_end*Sal_treat, data = data)
summary(PhenDOC)

par(mfrow = c(1,1), mar = c (4,5,2,2))

p <- ggplot(data=data, aes(x=DOC_mg_L_end, y = phenol_perc_end, shape = fSite, color = Sal_treat)) + geom_point() + geom_smooth(method = "lm")
p

