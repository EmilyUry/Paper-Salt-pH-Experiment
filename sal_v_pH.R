

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")

data <- read.csv("chapter4_mater.csv", head = T)
head(data)
plot(data$Phenol_mg_L_init, data$ugC_CH4_gc_hr)


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

sdata <- data[which(data$pH_n == 'n'),]
pdata <- data[which(data$Sal_treat == 0),]

par(mar = c(5,6,2,2))




plot(sdata$sal_init, sdata$sal_end)

plot(data$sal_init, data$sal_end, ylab = "Salinity final extract", 
     xlab = "Salinity initial rinsate",
     ylim = c(-1,3), xlim = c(-1,11), pch = 16, 
     col = c("red", "yellow", "blue")[data$pH_treat])


plot(data$pH_init, data$pH_end, ylab = "Salinity final extract", 
     xlab = "Salinity initial rinsate",
     ylim = c(5,7), xlim = c(3,7), pch = c(16, 21)[data$Site], 
     col = c("red", "yellow", "blue")[data$Sal_treat])

site3 <- data[which(data$Site == "3"),]
site5 <- data[which(data$Site == "5"),]

col = c("red", "red", "red", "purple", "purple", "purple",
        "blue", "blue", "blue")
col = c("red", "purple", "blue","red", "purple", "blue",
        "red", "purple", "blue")


{par(mfrow = c(2,2), mar = c(4,4,2,1))
boxplot(site3$pH_init~site3$pH_treat*site3$Sal_treat,
        ylim = c(3,7), border = col,
        main = "Site 3 (acidic)", 
        ylab = "pH (initial rinse)", xlab = "Salinity Treatment (ppt) ",
        xaxt = 'n')
abline(v = 3.5)
abline(v = 6.5)
axis(1, c(2,5,8), c("0", "2.5","10"))
legend(8,7, c("5.5", "7.2", "8.8"), pch = 22, 
       col = c("red", "purple", "blue"), cex = 0.7, 
       title = "pH")
boxplot(site5$pH_init~site5$pH_treat*site5$Sal_treat,
        ylim = c(3,7), border = col, 
        main = "Site 5 (cypress)", 
        ylab = " ", xlab = "Salinity Treatment (ppt) ", 
        xaxt = 'n')
abline(v = 3.5)
abline(v = 6.5)
axis(1, c(2,5,8), c("0", "2.5","10"))

#par(mfrow = c(2,2))
boxplot(site3$pH_end~site3$pH_treat*site3$Sal_treat,
        ylim = c(3,7), border = col, 
        ylab = "pH (final extract)", xlab = "Salinity Treatment (ppt) ", 
        xaxt = 'n')
abline(v = 3.5)
abline(v = 6.5)
axis(1, c(2,5,8), c("0", "2.5","10"))
boxplot(site5$pH_end~site5$pH_treat*site5$Sal_treat,
        ylim = c(3,7), border = col, 
        ylab = " ", xlab = "Salinity treatment (ppt) ", 
        xaxt = 'n')
abline(v = 3.5)
abline(v = 6.5)
axis(1, c(2,5,8), c("0", "2.5","10"))

}
#






##### salt treatments

{par(mfrow = c(2,2), mar = c(4,4,2,1))
  boxplot(site3$sal_init~site3$pH_treat*site3$Sal_treat,
          ylim = c(0,11), border = col,
          main = "Site 3 (acidic)", 
          ylab = "Sal. (initial rinse)", xlab = "Salinity Treatment (ppt)", 
          xaxt = 'n')
  abline(v = 3.5)
  abline(v = 6.5)
  axis(1, c(2,5,8), c("0", "2.5","10"))
  legend(8,6, c("5.5", "7.2", "8.8"), pch = 22, 
         col = c("red", "purple", "blue"), cex = 0.7, 
         title = "pH")
  boxplot(site5$sal_init~site5$pH_treat*site5$Sal_treat,
          ylim = c(0,11), border = col, 
          main = "Site 5 (cypress)", 
          ylab = " ", xlab = "Salinity Treatment (ppt) ", 
          xaxt = 'n')
  abline(v = 3.5)
  abline(v = 6.5)
  axis(1, c(2,5,8), c("0", "2.5","10"))
  
  #par(mfrow = c(2,2))
  boxplot(site3$sal_end~site3$pH_treat*site3$Sal_treat,
          ylim = c(0,2), border = col, 
          ylab = "Sal. (final extract)", xlab = "Salinity Treatment (ppt) ", 
          xaxt = 'n')
  abline(v = 3.5)
  abline(v = 6.5)
  axis(1, c(2,5,8), c("0", "2.5","10"))
  boxplot(site5$sal_end~site5$pH_treat*site5$Sal_treat,
          ylim = c(0,2), border = col, 
          ylab = " ", xlab = "Salinity treatment (ppt) ", 
          xaxt = 'n')
  abline(v = 3.5)
  abline(v = 6.5)
  axis(1, c(2,5,8), c("0", "2.5","10"))
  
}






###############
## VERSION 1 - x-axis categorical
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(data, varname="pH_init", 
                    groupnames=c("pH_treat", "Sal_treat", "Site"))
labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")

ggplot(df2, aes(x=Sal_treat, y=pH_init, group=pH_treat, shape = pH_treat)) + 
  geom_pointrange(aes(ymin=pH_init-sd, ymax=pH_init+sd)) +
  theme_bw() +
  facet_grid(. ~ Site, labeller = labeller(Site = labs)) +
  xlab("Salinity Treatment (ppt)") +
  ylab("pH (filtrate)")


## VERSION 2 - x axis numeric

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      var = var(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(data, varname="pH_init", 
                    groupnames=c("pH_treat", "Sal_treat", "Site"))
df3 <- data_summary(data, varname = "sal_init", groupnames=c("pH_treat", "Sal_treat", "Site"))
df2$sal_init <- df3$sal_init
df2$salty <- as.numeric(as.character(df2$Sal_treat))
df2$se <- sqrt(df2$var/3)
df2$salt_se <- sqrt(df3$var/3)

labs <- c("Ponzer muck", "Hyde loam")
names(labs) <- c("3", "5")
line <- c(rep(1, 6), rep(3,6), rep(2, 6))

ggplot(df2, aes(x=sal_init, y=pH_init, shape = factor(pH_treat))) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=pH_init-se, ymax=pH_init+se), width = 0.3, size = 0.7) +
  #geom_segment(aes(x = sal_init-salt_se, xend=sal_init+salt_se, y = pH_init, yend = pH_init)) +
  geom_path(linetype = line) +
  theme_bw() +
  facet_grid(. ~ Site, labeller = labeller(Site = labs)) +
  xlab("Salinity Treatment (ppt)") +
  ylab("pH (filtrate)") +
  labs(shape = "pH Treatment") +
  theme(legend.position = c(.3, .75))








##color
# ggplot(df2, aes(x=salty, y=pH_init, shape = factor(pH_treat), col = factor(pH_treat))) + 
#   geom_point(size = 2) +
#   scale_color_viridis(discrete = TRUE, begin = 0, end =0.85,  option ="D") +
#   geom_errorbar(aes(ymin=pH_init-se, ymax=pH_init+se), width = 0.3, size = 0.7) +
#   geom_path(linetype = line) +
#   theme_bw() +
#   facet_grid(. ~ Site, labeller = labeller(Site = labs)) +
#   xlab("Salinity Treatment (ppt)") +
#   ylab("pH (filtrate)") +
#   labs(shape = "pH Treatment") +
#   theme(legend.position = c(.3, .75))

