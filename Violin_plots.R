#Loading data

setwd("~/GitHub/larvae-and-sound")
data2 <- read.csv2(file="sound_data2.csv", head=TRUE, sep=",")
colnames(data2)[1]<- "date"
data2 <- data2[, 1:8]
data2 <- data2[1:100,]
data2$treatment <- as.factor(data2$treatment)
data2$proportion_settled <- as.numeric(data2$proportion_settled)
data2$date <- as.factor(data2$date)
library(ggplot2)

#ANOVA all data
model2 <- aov(proportion_settled ~ treatment, data= data2)
summary(model2)
TukeyHSD(model2)

leveneTest(model2)
hist(resid(model2), breaks = 20, col = "gray", xlab = "Residuals")
qqnorm(resid(model2))
qqline(resid(model2))
plot(model2)



#violin plot
ggplot(data2, aes(x = treatment, y = proportion_settled, fill = treatment)) + 
  geom_violin() +
  stat_summary(fun.data = mean_sdl) +
  xlab("Treatment All") + ylab("Settled") +
  theme_classic()


#ANOVA Batch one
data2$date <-as.factor(data2$date)
data_batch1 <- subset(data2, date == "3-Mar")
model3 <- aov(proportion_settled ~ treatment, data= data_batch1)
summary(model3)
TukeyHSD(model3)

#violin plot for batch 1
ggplot(data_batch1, aes(x = treatment, y = proportion_settled, fill = treatment)) + 
  geom_violin() +
  stat_summary(fun.data = mean_sdl) +
  xlab("Treatment batch 1") + ylab("Settled") +
  theme_classic()

#ANOVA Batch two 
data_batch2 <- subset(data2, date == "4-Mar")
model4 <- aov(proportion_settled ~ treatment, data= data_batch2)
summary(model4)
TukeyHSD(model4)

#violin plot for batch two
ggplot(data_batch2, aes(x = treatment, y = proportion_settled, fill = treatment)) + 
  geom_violin() +
  stat_summary(fun.data = mean_sdl) +
  xlab("Treatment batch 2") + ylab("Settled") +
  theme_classic()

#ANOVA Batch three 
data_batch3 <- subset(data2, date == "5-Mar")
model5 <- aov(proportion_settled ~ treatment, data= data_batch3)
summary(model5)
TukeyHSD(model5)

#plot for 3 
ggplot(data_batch3, aes(x = treatment, y = proportion_settled, fill = treatment)) + 
  geom_violin() +
  stat_summary(fun.data = mean_sdl) +
  xlab("Treatment batch 3") + ylab("Settled") +
  theme_classic()

#ANOVA Batch four
data_batch4 <- subset(data2, date == "7-Mar")
model6 <- aov(proportion_settled ~ treatment, data= data_batch4)
summary(model6)
TukeyHSD(model6)

#plots for 4 
ggplot(data_batch4, aes(x = treatment, y = proportion_settled, fill = treatment)) + 
  geom_violin() +
  stat_summary(fun.data = mean_sdl) +
  xlab("Treatment batch 4") + ylab("Settled") +
  theme_classic()

