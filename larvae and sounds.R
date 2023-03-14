install.packages("dplyr")
install.packages("Matrix")
install.packages("lme4")
install.packages("ggeffects")
library(dplyr)
library(Matrix)
library(lme4)
library(ggeffects)


#loading data 

data <- read.csv2(file="sound_data.csv", head=TRUE, sep=",")
colnames(data)[1]<- "date"
data <- data[, 1:7]
data <- data[1:100,]
data$tank <- as.factor(data$tank)
data$date <- as.factor(data$date)
data$speaker <- as.factor(data$speaker)
data$cup_position <- as.factor(data$cup_position)


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
model <-glmer(settled ~ treatment + (1 | date), data = data, family = binomial)
car::Anova(model, type=2)

#post hoc  
marginal <-lsmeans(model, ~treatment)
pairs(marginal, adjust="tukey")

#checking for an effect from random variables
#adding date as a fixed effect

model_date <-glmer(settled ~ date + (1 | treatment), data = data, family = binomial)
car::Anova(model_date, type=2)
marginal_date <-lsmeans(model_date, ~date)
pairs(marginal_date, adjust="tukey")

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
data_batch2 <- subset(data, date == "4-Mar")
data_batch3 <- subset(data, date == "5-Mar")
data_batch4 <- subset(data, date == "7-Mar")
data_batch234 <- rbind(data_batch2, data_batch3, data_batch4)

model_cup <-glmer(settled ~ cup_position + (1 | treatment) + (1 | tank) + (1 | date), data = data_batch234, family = binomial)
car::Anova(model_cup, type=2)
marginal_cup <-lsmeans(model_cup, ~ cup_position)
pairs(marginal_cup, adjust="tukey")

