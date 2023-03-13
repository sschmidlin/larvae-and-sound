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



# Calculate response variable from input data
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

# general liner model
model <-glmer(settled ~ "no sound" + "boat" + "healthy reef" + "off reef" + "boat + reef" + (1 | tank), data = data, family = binomial)
summary(model)

