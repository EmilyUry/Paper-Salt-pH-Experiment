

### lmme model

library(lme4)
#citation('lme4')


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")


data <- read.csv("chapter4_mater.csv", head = T)
head(data)
names(data) <- c("Site", "Sal_treat", "pH_treat","pH_n","Sample_no","SM_init",
                 "C_init","sal_init","pH_init" ,"DOC_init","TDN_mg_L_init",
                 "Phenol_mg_L_init", "SUVA254_init", "C3_s","C3_c",
                 "CH4_s", "CH4_c", "SM_end","C_end", "pH_end",
                 "sal_end","Phenol_mg_L_end", "SUVA254_end", "DOC_end",
                 "TDN_mg_L_end", "C21_c")

data$Site <- as.factor(data$Site)
data$pH_treat <- as.factor(data$pH_treat)
data$Sal_treat <- as.factor(data$Sal_treat)

##outlier removal
data[11,12] <- NA
data[11,13] <- NA
  
  
# treat site fixed categorical variable
  
model1 <- lmer(DOC_init ~ pH_treat + Sal_treat + (1|Site), data = data, REML = F)
model2 <- lmer(DOC_init ~ Sal_treat + (1|Site), data = data, REML = F)
model3 <- lmer(DOC_init ~ pH_treat + (1|Site), data = data, REML = F)
model4 <- lmer(DOC_init ~ Sal_treat + Sal_treat*pH_treat + (1|Site), data = data, REML = F)

null <- lmer(DOC_init ~ 1 + (1|Site), data = data, REML = F)
anova(model1, model2)
anova(model2, null)
anova(model3, null)
anova(model4, model2)







model1 <- lmer(DOC_end ~ pH_treat + Sal_treat + (1|Site), data = data, REML = F)
model2 <- lmer(DOC_end ~ Sal_treat + (1|Site), data = data, REML = F)
model3 <- lmer(DOC_end ~ pH_treat + (1|Site), data = data, REML = F)
model4 <- lmer(DOC_end ~ Sal_treat + Sal_treat*pH_treat + (1|Site), data = data, REML = F)

null <- lmer(DOC_end ~ 1 + (1|Site), data = data, REML = F)
anova(model1, model2)
anova(model2, null)
anova(model3, null)
anova(model4, model2)




model1 <- lmer(C3_c ~ pH_treat + Sal_treat + (1|Site), data = data, REML = F)
model2 <- lmer(C3_c ~ Sal_treat + (1|Site), data = data, REML = F)
model3 <- lmer(C3_c ~ pH_treat + (1|Site), data = data, REML = F)
model4 <- lmer(C3_c ~ Sal_treat + Sal_treat*pH_treat + (1|Site), data = data, REML = F)

null <- lmer(C3_c ~ 1 + (1|Site), data = data, REML = F)
anova(model1, model2)
anova(model2, null)
anova(model3, null)
anova(model4, model2)
