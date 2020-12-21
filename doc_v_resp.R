




#### DOC vs Resp


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)

data$Site <- as.factor(data$Site)
data$pH_treat <- as.factor(data$pH_treat)
data$Sal_treat <- as.factor(data$Sal_treat)





p <- ggplot(data, aes(DOC_mg_L_end, total, shape = Sal_treat))
p  + geom_point() + theme_bw() + facet_grid(Site~pH_treat) + scale_shape_manual(values = c(1, 15, 3))



p <- ggplot(data, aes(DOC_mg_L_init, total, shape = Sal_treat))
p  + geom_point() + theme_bw() + facet_grid(Site~pH_treat) + scale_shape_manual(values = c(1, 15, 3))


            

#scale_linetype_manual(values=c("solid", "solid", "solid", "dashed", "dashed", "dashed", "dotted", "dotted", "dotted"))




S5 <- data[which(data$Site == "5"),]
head(S5)
p <- ggplot(S5, aes(Day, flux, linetype = Group))
p +  stat_smooth() + theme_bw() + facet_grid(alk..treatment~.) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "dashed", "dashed", "dashed", "dotted", "dotted", "dotted")) +
  ylim(0,2150)





df <- read.csv("chapter4_mater.csv", head = T)
head(df)

df$Site <- as.factor(df$Site)
df$pH_treat <- as.factor(df$pH_treat)
df$Sal_treat <- as.factor(df$Sal_treat)

site3 <- df[which(df$Site == "3"),]
site5 <- df[which(df$Site == "5"),]


p <- ggplot(df, aes(x=Sal_treat, y=total)) + 
  geom_boxplot() 
p + theme_bw() + facet_grid(pH_treat ~ Site)



#### stats


anova <- aov(total ~ Sal_treat*pH_treat, data = site3)
TukeyHSD((anova))

##multicomp
#dht <- glht(anova, linfct = mcp(Sal_treat = "Tukey"))
#summary(dht)
library(agricolae)
intx <- with(site3, interaction(pH_treat, Sal_treat))
mod <- aov(total ~ intx, data = site3)
out <- HSD.test(mod,"intx", group=FALSE)
print(out$comparison)
out <- HSD.test(mod,"intx", group=TRUE)
out

