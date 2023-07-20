install.packages("dplyr")
install.packages("Matrix")
install.packages("lme4")
install.packages("ggeffects")
install.packages("lsmeans")
install.packages("emmeans")
library(ggplot2)
library(dplyr)
library(Matrix)
library(lme4)
library(ggeffects)
library(emmeans)
library(lsmeans)

#loading data 
setwd("~/GitHub/larvae-and-sound")
data <- read.csv2(file="sound_data.csv", head=TRUE, sep=",")
colnames(data)[1]<- "date"
data <- data[1:100,]
data$tank <- as.factor(data$tank)
data$date <- as.factor(data$date)
data$speaker <- as.factor(data$speaker)
data$cup_position <- as.factor(data$cup_position)
data$aci <- as.numeric(data$aci)
data$adi <- as.numeric(data$adi)
data$aei <- as.numeric(data$aei)
data$date <- as.character(data$date)
data$speaker <- ifelse(data$speaker == "x", "0", data$speaker)
data$speaker <- ifelse(data$speaker == "5", "0", data$speaker)
data$spl <- as.numeric(data$spl)
data$low_freq <-as.numeric(data$low_freq)
data$high_freq <-as.numeric(data$high_freq)
data$mid_freq <-as.numeric(data$mid_freq)
  
# Calculate response variable from input data, adding binary data settled =1 not-settled = 0 
mult_s <- rep(1:nrow(data), data[, 'settled'])
mult_u <- rep(1:nrow(data), data[, 'unsettled'])
data_s <- data[mult_s,]
data_s[, 'settled'] <- 1
data_s <- data_s[, !(names(data_s) %in% c('unsettled'))]
data_u <- data[mult_u,]
data_u[, 'unsettled'] <- 0
data_u <- data_u[, !(names(data_u) %in% c('settled'))]
colnames(data_u)[5] <- 'settled'
data <- rbind(data_s, data_u)

# GLM
model <-glmer(settled ~ treatment + (1|date), data = data, family = binomial)
car::Anova(model, type=2)

#post hoc  
marginal <-lsmeans(model, ~treatment)
pairs(marginal, adjust="tukey")

#visuals 
#prediction plot
m <- ggpredict(model, terms = c("treatment"))
plot(m)+
  geom_point() +
  labs(title = "", x = 'Treatment', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))



#checking for an effect from random variables
#adding date as a fixed effect
model_date <-glmer(settled ~ date + (1 | treatment), data = data, family = binomial)
car::Anova(model_date, type=2)
marginal_date <-lsmeans(model_date, ~date)
pairs(marginal_date, adjust="tukey")

#prediction plot
m1 <- ggpredict(model_date, terms = c("date"))
plot(m1)+
  geom_point() +
  labs(title = "", x = 'Treatment', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20))


#adding tank as a fixed effect 
model_tank <-glmer(settled ~ tank + (1 | treatment) + (1 | speaker) + (1 | date), data = data, family = binomial)
car::Anova(model_tank, type=2)
marginal_tank <-lsmeans(model_tank, ~ tank)
pairs(marginal_tank, adjust="tukey")

#adding speaker as a fixed effect
model_speaker <-glmer(settled ~ speaker + (1 | treatment) + (1 | tank) + (1 | date), data = data, family = binomial)
car::Anova(model_speaker, type=2)
marginal_speaker <-lsmeans(model_speaker, ~ speaker)
pairs(marginal_speaker, adjust="tukey")

#adding cup position as fixed effect
#only batches 2,3,4 were used in this because we didn't record cup position for the first day of the experiment
data_batch2 <- subset(data, date == "4-Mar")
data_batch3 <- subset(data, date == "5-Mar")
data_batch4 <- subset(data, date == "7-Mar")
data_batch234 <- rbind(data_batch2, data_batch3, data_batch4)

model_cup <-glmer(settled ~ cup_position + (1 | treatment) + (1 | tank) + (1 | date), data = data_batch234, family = binomial)
car::Anova(model_cup, type=2)
marginal_cup <-lsmeans(model_cup, ~ cup_position)
pairs(marginal_cup, adjust="tukey")

#conclusion, as tank, speakers, and cup position are all non significant we can exclude these from consideration in the model.
#date does cause a significant difference. 



#only boat sounds over date 

data_boat1 <- subset(data, treatment == "boat")
data_boat1$boat <- data_boat1$date
data_boat1 <- data_boat1 %>% 
  mutate(boat = recode(boat, "3-Mar" = "Faulbaums"))
data_boat1 <- data_boat1 %>% 
  mutate(boat = recode(boat, "4-Mar" = "Grafton"))
data_boat1 <- data_boat1 %>% 
  mutate(boat = recode(boat, "5-Mar" = "Fairplay boats"))
data_boat1 <- data_boat1 %>% 
  mutate(boat = recode(boat, "7-Mar" = "Buitenratel"))


model_boat1 <-glm(settled ~ boat, data = data_boat1, family = binomial)
car::Anova(model_boat1, type=2)
marginal_boat1 <-lsmeans(model_boat1, ~ boat)
pairs(marginal_boat1, adjust="tukey")

m2 <- ggpredict(model_boat1, terms = c("boat"))
plot(m2)+
  geom_point() +
  labs(title = "", x = 'Boat', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))


#only healthy reef sound over date

data_healthy <- subset(data, treatment == "healthy reef")
model_healthy <- glm(settled ~ date, data = data_healthy, family = binomial)
car::Anova(model_healthy, type=2)
marginal_healthy <-lsmeans(model_healthy, ~ date)
pairs(marginal_healthy, adjust="tukey")

plot(m3)+
  geom_point() +
  labs(title = "", x = 'Healthy reef', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))





#only off reef sound over date
data_off <- subset(data, treatment == "off reef")
model_off <- glm(settled ~ date, data = data_off, family = binomial)
car::Anova(model_off, type=2)
marginal_off <-lsmeans(model_off, ~ date)
pairs(marginal_off, adjust="tukey")

m4 <- ggpredict(model_off, terms = c("date"))
plot(m4)+
  labs(x = 'off reef', 
       y= 'Larvae Settled (%)',
       title = "")

#only no sound 
data_nosound <- subset(data, treatment == "no sound")
model_nosound <- glm(settled ~ date, data = data_nosound, family = binomial)
car::Anova(model_nosound, type=2)
marginal_nosound <-lsmeans(model_nosound, ~ date)
pairs(marginal_nosound, adjust="tukey")

m5 <- ggpredict(model_nosound, terms = c("date"))
plot(m5)+
  labs(x = 'no sound', 
       y= 'Larvae Settled (%)',
       title = "")

#only reef + boat 
data_RB <- subset(data, treatment == "healthy reef + boat")
model_RB <- glm(settled ~ date, data = data_RB, family = binomial)
car::Anova(model_RB, type=2)
marginal_RB <-lsmeans(model_RB, ~ date)
pairs(marginal_RB, adjust="tukey")

m6 <- ggpredict(model_RB, terms = c("date"))
plot(m6)+
  labs(x = 'Reef + Boat', 
       y= 'Larvae Settled (%)',
       title = "")


#adding the spl

#normalizing spl
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
data$spl <- normalize(data$spl)

model <-glmer(settled ~ treatment + spl + (1|date), data = data, family = binomial)


car::Anova(model, type=2)
marginal <-lsmeans(model, ~treatment)
pairs(marginal, adjust="tukey")

#boxplots 
boxplot(spl ~ treatment, data = data)
boxplot(adi ~ treatment, data= data)
boxplot(aei ~ treatment, data= data)
boxplot(spl ~ date, data = data_boat1)
boxplot(aci ~ date, data= data_boat1)
boxplot(aci ~ date, data = data_healthy)
boxplot(low_freq ~ treatment, data= data)
boxplot(mid_freq ~ treatment, data= data)
boxplot(high_freq ~ treatment, data= data)
