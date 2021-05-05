

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)
data$fSite <- as.factor(data$Site)
data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)

# data <- data[which(data$Site == "5"),]
# Site5 <- data[which(data$Site == "5"),]


## Check these responses
data$response <- data$sal_init ## not normal
data$response <- data$sal_end ## not normal
data$response <- data$pH_init ## NORMAL
data$response <- data$pH_end ## not normal

data$response <- data$ugC.CO2_hr_gc ## not normal
data$response <- data$T21_ug_CO2_gc 
data$response <- data$DOC_mg_L_end ## not normal

data$response <- data$Phenol_mg_L_end/data$DOC_mg_L_end
data$response <- data$SUVA254_end/data$DOC_mg_L_end*100


data$response <- data$C_end ## NORMAL
data$response <- data$ugC.CO2_hr_gds ## not normal

data$response <- data$Phenol_mg_L_end ## not normal
data$response <- data$SUVA254_end ## not normal


shapiro.test(data$response)

### model
### TWO-WAY ANOVA
res.aov <- aov(response ~ Sal_treat, data = data)
# Extract the residuals
aov_residuals <- residuals(object = res.aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals ) ### if p is less than 0.5, the residuals are not normal
## use Kruskal-wallis test for non parametric data
hist(data$response)


### TWO-WAY ANOVA
res.aov <- aov(response ~ pH_treat, data = data)
# Extract the residuals
aov_residuals <- residuals(object = res.aov)
summary(res.aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals ) ### if p is less than 0.5, the residuals are not normal
## use Kruskal-wallis test for non parametric data
hist(data$response)
