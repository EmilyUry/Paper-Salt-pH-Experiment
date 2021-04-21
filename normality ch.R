

## normality check


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)
data$fSite <- as.factor(data$Site)
data$Sal_treat <- as.factor(data$Sal_treat)
data$pH_treat <- as.factor(data$pH_treat)


### Response

## Check these responses
data$response <- data$sal_init ## not normal
data$response <- data$sal_end ## not normal
data$response <- data$pH_init ## not normal
data$response <- data$pH_end ## not normal
data$response <- data$ugC.CO2_hr_gc ## not normal
data$response <- data$T21_ug_CO2_gc 
data$response <- data$DOC_mg_L_end ## not normal

data$response <- data$Phenol_mg_L_end/data$DOC_mg_L_end
data$response <- data$SUVA254_end/data$DOC_mg_L_end*100


data$response <- data$C_end ## NORMAL
data$response <- data$ugC.CO2_hr_gds 

data$response <- data$Phenol_mg_L_end ## not normal
data$response <- data$SUVA254_end ## not normal





### model
### TWO-WAY ANOVA
res.aov <- aov(response ~ Sal_treat*fSite, data = data)


# Extract the residuals
aov_residuals <- residuals(object = res.aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )





## Kruskal.test

Site3 <- data[which(data$Site == "3"),]
Site5 <- data[which(data$Site == "5"),]

Site3$response <- Site3$C_end ## NORMAL
Site3$response <- Site3$DOC_mg_L_end ##  NORMAL
Site3$response <- Site3$Phenol_mg_L_end ## NORMAL
Site3$response <- Site3$SUVA254_end ## NORMAL
Site3$response <- Site3$sal_init ## not normal
Site3$response <- Site3$sal_end ## not normal
Site3$response <- Site3$ugC.CO2_hr_gc ## not normal
Site3$response <- Site3$ugC.CO2_hr_gc_pr ## not normal

kt <- kruskal.test(response ~ Sal_treat, data = Site3)
kt
pairwise.wilcox.test(Site3$response, Site3$Sal_treat)

Site5$response <- Site5$C_end ## NORMAL
Site5$response <- Site5$DOC_mg_L_end ##  NORMAL
Site5$response <- Site5$Phenol_mg_L_end ## NORMAL
Site5$response <- Site5$SUVA254_end ## not Normal
Site5$response <- Site5$sal_init ## not normal
Site5$response <- Site5$sal_end ## not normal
Site5$response <- Site5$ugC.CO2_hr_gc ## not normal
Site5$response <- Site5$ugC.CO2_hr_gc_pr ## not normal


kruskal.test(response ~ Sal_treat, data = Site5)
pairwise.wilcox.test(Site5$response, Site5$Sal_treat)





### for the non-parametric responses with two groups (Site and Salt Treatment)
### we use the ScheirerRayHare extension of Kruskal wallis test in the package 'rcompanion'


library(rcompanion)

scheirerRayHare(response ~ Sal_treat*fSite, data = data)







## site wise Shapiro test

Site3 <- data[which(data$Site == "3"),]
Site5 <- data[which(data$Site == "5"),]

Site3$response <- Site3$C_end ## NORMAL
Site3$response <- Site3$DOC_mg_L_end ##  NORMAL
Site3$response <- Site3$Phenol_mg_L_end ## NORMAL
Site3$response <- Site3$SUVA254_end ## NORMAL
Site3$response <- Site3$sal_init ## not normal
Site3$response <- Site3$sal_end ## not normal
Site3$response <- Site3$ugC.CO2_hr_gc ## not normal
Site3$response <- Site3$ugC.CO2_hr_gc_pr ## not normal

res.aov <- aov(response ~ Sal_treat, data = Site3)


# Extract the residuals
aov_residuals <- residuals(object = res.aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

### Site 5




Site5$response <- Site5$C_end ## NORMAL
Site5$response <- Site5$DOC_mg_L_end ##  NORMAL
Site5$response <- Site5$Phenol_mg_L_end ## NORMAL
Site5$response <- Site5$SUVA254_end ## not Normal
Site5$response <- Site5$sal_init ## not normal
Site5$response <- Site5$sal_end ## not normal
Site5$response <- Site5$ugC.CO2_hr_gc ## not normal
Site5$response <- Site5$ugC.CO2_hr_gc_pr ## not normal

res.aov <- aov(response ~ Sal_treat, data = Site5)

# Extract the residuals
aov_residuals <- residuals(object = res.aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )


