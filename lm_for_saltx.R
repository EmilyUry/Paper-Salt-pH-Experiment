


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)

data$Site <- as.factor(data$Site)
data$pH_treat <- as.factor(data$pH_treat)
data$Sal_treat <- as.factor(data$Sal_treat)

##outlier removal
data[11,12] <- NA
data[11,13] <- NA


data$SUVA_perc_init <- data$SUVA254_init/data$DOC_mg_L_init*100
data$SUVA_perc_end <- data$SUVA254_end/data$DOC_mg_L_end*100

data$phenol_perc_init <- data$Phenol_mg_L_init/data$DOC_mg_L_init*100
data$phenol_perc_end <- data$Phenol_mg_L_end/data$DOC_mg_L_end*100



### basic model structure
## response of interest:

# DOC_mg_L_init
# DOC_mg_L_end
# C_end
# ugC.CO2_hr_gc
# total (this is the 21 day total respiration)
# SUVA254_end
# Phenol_mg_L_end

mod1 <- lm(DOC_mg_L_end ~ Site*pH_treat*Sal_treat, data = data)
summary(mod1)
mod1
anova(mod1)


