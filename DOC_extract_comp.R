



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








#### Ph

newd$SUVA_per <- newd$SUVA254/newd$DOC_mg_L
newd$phenol_per <- newd$Phenol_mg_L/newd$DOC_mg_L


##DOC/Phenol/Suva
newd$response <- newd$phenol_per ## 
SiteSal <- lm(response ~ Extract*Salinity*Site, data = newd)
summary(SiteSal)
# plot
emmip(SiteSal, Salinity ~ Extract | Site, CIs = TRUE, nesting.order = TRUE, 
      xlab = "Extract", ylab = "Phenolics (per DOC)")  ## mg phenolics/mg DOC

##DOC/Phenol/Suva
newd$response <- newd$SUVA_per ##  
SiteSal <- lm(response ~ Extract*Salinity*Site, data = newd)
summary(SiteSal)
# plot
emmip(SiteSal, Salinity ~ Extract | Site, CIs = TRUE, nesting.order = TRUE, 
      xlab = "Extract", ylab = "SUVA254 ")  ## L / mg DOC / cm (this is the correct units)





