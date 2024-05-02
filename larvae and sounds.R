#install.packages("dplyr")
#install.packages("Matrix")
#install.packages("lme4")
#install.packages("ggeffects")
#install.packages("lsmeans")
#install.packages("emmeans")
library(ggplot2)
library(dplyr)
library(Matrix)
library(lme4)
library(ggeffects)
library(emmeans)
library(lsmeans)
library(cowplot)


################
#              #
# Loading data #
#--------------#
#              #
################
data <- read.csv2(file="./data2.csv", head=TRUE, sep=",")


#############################
#                           #
# Prepare data for analysis #
#---------------------------#
#                           #
#############################
# Turn treatment into a factor and assign a specific order for the factor levels
treatment_order <- c("reef", "reef + vessel", "vessel", "off reef", "no sound") 
data$treatment <- factor(data$treatment, levels = treatment_order)

# Turn speaker, tank, data and cup_position into factors
data$tank <- as.factor(data$tank)
data$date <- as.factor(data$date)
data$speaker <- as.factor(data$speaker)
data$cup_position = as.factor(data$cup_position)

# Make a new factor that includes cup identity
cup_1 = as.numeric(data$cup_position)
for (i in 1:nrow(data)){
  if (data$date[i] == '3-Mar'){
    cup_1[i] = data$larvae_cup[i]
  }
}
data$cup <- as.factor(paste(data$date, data$tank, cup_1, sep = "_"))


##########################################
#                                        #
# Evaluating which predictors to include #
#----------------------------------------#
#                                        #
##########################################

# Test inclusion of speaker effect
#---------------------------------
data_without_speaker = subset(data, speaker!=1) #exclude no sound treatments from data because they do not have a speaker
data_without_speaker$speaker <- droplevels(data_without_speaker$speaker, exclude = "1")

# Define the initial model
base_model <- glmer(settled ~ treatment + date + (1|treatment:date) + (1|cup), data = data_without_speaker, family = binomial)

# Define the updated model
speaker_model <- glmer(settled ~ treatment + date + speaker + (1|treatment:date) + (1|cup), data = data_without_speaker, family = binomial)
anova(base_model, speaker_model)

m <- ggpredict(speaker_model, terms = c("speaker"))
plot(m)+
  geom_point() +
  labs(title = "", x = 'Speaker', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))


# Test inclusion of tank effect
#------------------------------
# Define the initial model
base_model <- glmer(settled ~ treatment + date + (1|treatment:date) + (1|cup), data = data, family = binomial)

# Define the updated model
tank_model <- glmer(settled ~ treatment + date + tank + (1|treatment:date) + (1|cup), data = data, family = binomial)
anova(base_model, tank_model)

m <- ggpredict(tank_model, terms = c("tank"))
plot(m)+
  geom_point() +
  labs(title = "", x = 'Tank', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))


# Test inclusion of the effect of cup position
#---------------------------------------------
data_without_cup_position <- subset(data, !is.na(cup_position)) # Exclude rows without cup position

# Define the initial model
base_model <- glmer(settled ~ treatment + date + (1|treatment:date) + (1|cup), data = data_without_cup_position, family = binomial)

# Define the updated model
cup_position_model <- glmer(settled ~ treatment + date + cup_position + (1|treatment:date) + (1|cup), data = data_without_cup_position, family = binomial)
anova(base_model, cup_position_model)

m <- ggpredict(cup_position_model, terms = c("cup_position"))
plot(m)+
  geom_point() +
  labs(title = "", x = 'Cup position', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))


#######################
#                     #
# Run the final model #
#---------------------#
#                     #
#######################

model <- glmer(settled ~ treatment + date + (1|treatment:date) + (1|cup), data = data, family = binomial)
marginal <-lsmeans(model, ~treatment)
pairs(marginal, adjust="tukey")

m <- ggpredict(base_model, terms = c("treatment", "date"))
plot(m)+
  geom_point() +
  labs(title = "", x = 'Treatment', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))














# Old code, do not run

summary(base_model)
car::Anova(base_model, type=2)
marginal <-lsmeans(base_model, ~treatment)
pairs(marginal, adjust="tukey")

m <- ggpredict(base_model, terms = c("treatment", "date"))
plot(m)+
  geom_point() +
  labs(title = "", x = 'Treatment', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))

new_data <- subset(data, !is.na(cup_position))
new_data2 = subset(data, speaker!=1)

model <- glmer(settled ~ treatment + date + (1|treatment:date) + (1|cup), data = new_data2, family = binomial)
model_1 <- glmer(settled ~ treatment + date + speaker + (1|treatment:date) + (1|cup), data = new_data2, family = binomial)
anova(model, model_1)
sum = summary(model)
sum$AICtab[1]

car::Anova(model, type=2)
marginal <-lsmeans(model, ~treatment)
pairs(marginal, adjust="tukey")

m <- ggpredict(model, terms = c("treatment", "date"))
plot(m)+
  geom_point() +
  labs(title = "", x = 'Treatment', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))

# List of candidate variables
candidate_variables <- c("cup_position", "speaker", "tank", "date")

# Initialize empty lists to store selected fixed and random effects
selected_fixed_effects <- c("treatment")  
selected_random_effects <- c()

# Forward selection procedure
for (variable in candidate_variables) {
  # Add the current variable as a fixed effect
  fixed_formula <- paste("settled ~", paste(selected_fixed_effects, collapse = " + "), "+", variable,
                         if (length(selected_random_effects) > 0) paste("+ (1|", paste(selected_random_effects, collapse = " + "), ")", sep = ""),
                         "+ (1|dummy_group)", sep = "")
  
  # Fit the model with the added fixed effect
  fixed_model <- glmer(as.formula(fixed_formula), data = data, family = binomial)
  
  # Add the current variable as a random effect
  random_formula <- paste("settled ~", paste(selected_fixed_effects, collapse = " + "),
                          if (length(selected_fixed_effects) > 0) paste("+", paste(selected_fixed_effects, collapse = " + "), "+", variable, sep = "") else paste("+", variable),
                          if (length(selected_random_effects) > 0) paste("+ (1|", paste(selected_random_effects, collapse = " + "), ")", sep = ""),
                          "+ (1|dummy_group)", sep = "")
  
  # Fit the model with the added random effect
  random_model <- glmer(as.formula(random_formula), data = data, family = binomial)
  
  # Compare models using AIC
  if (AIC(fixed_model) < AIC(base_model)) {
    base_model <- fixed_model
    selected_fixed_effects <- c(selected_fixed_effects, variable)
  }
  
  if (AIC(random_model) < AIC(base_model)) {
    base_model <- random_model
    selected_random_effects <- c(selected_random_effects, variable)
  }
}

# Print the final selected fixed and random effects
print(selected_fixed_effects)

print(selected_random_effects)

# Print the final model summary
summary(base_model)















#old code, please ignore

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
library(cowplot)

#loading data  
setwd("~/GitHub/larvae-and-sound/data")
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

treatment_order <- c("reef", "reef + vessel", "vessel", "off reef", "no sound") 
data$treatment <- factor(data$treatment, levels = treatment_order)


#checking for an effect from random variables
#adding date as a fixed effect
model_date <-glmer(settled ~ date + treatment + (1|speaker) + (1|tank), data = data, family = binomial)
car::Anova(model_date, type=2)
marginal_date <-lsmeans(model_date, ~date)
pairs(marginal_date, adjust="tukey")


#adding tank as a fixed effect 
model_tank <-glmer(settled ~ tank + treatment + (1 | speaker) + (1 | date), data = data, family = binomial)
car::Anova(model_tank, type=2)
marginal_tank <-lsmeans(model_tank, ~ tank)
pairs(marginal_tank, adjust="tukey")


#adding speaker as a fixed effect
model_speaker <-glmer(settled ~ speaker + treatment + (1 | tank) + (1 | date), data = data, family = binomial)
car::Anova(model_speaker, type=2)
marginal_speaker <-lsmeans(model_speaker, ~ speaker)
pairs(marginal_speaker, adjust="tukey")


#adding cup position as fixed effect
#only batches 2,3,4 were used in this because we didn't record cup position for the first day of the experiment
data_batch2 <- subset(data, date == "4-Mar")
data_batch3 <- subset(data, date == "5-Mar")
data_batch4 <- subset(data, date == "7-Mar")
data_batch234 <- rbind(data_batch2, data_batch3, data_batch4)

model_cup <-glmer(settled ~ treatment + cup_position + (1 | tank) + (1 | date), data = data_batch234, family = binomial)
car::Anova(model_cup, type=2)
marginal_cup <-lsmeans(model_cup, ~ cup_position)
pairs(marginal_cup, adjust="tukey")

#conclusion: as tank, speakers, and cup position are all non significant we can exclude these from consideration in the model.
#date does cause a significant difference. 

#checking for an interaction effect from date and treatment 
model_interaction <-glmer(settled ~ treatment * date + (1|speaker) + (1|tank), data = data, family = binomial)
summary(model_interaction)
car::Anova(model_interaction, type=2)

marginal <-lsmeans(model_interaction, ~treatment)
pairs(marginal, adjust="tukey")

marginal <-lsmeans(model_interaction, ~date)
pairs(marginal, adjust="tukey")


# FINAL GLM MODEL

model <-glmer(settled ~ treatment + date + (1|cup_position), data = data, family = binomial)
car::Anova(model, type=2)
summary(model)

#post hoc  
marginal <-lsmeans(model, ~treatment)
pairs(marginal, adjust="tukey")

#creating prediction plots
m <- ggpredict(model, terms = c("treatment"))

plot(m)+
  geom_point() +
  labs(title = "", x = ' ', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))











#exploring why there is a significant effect from "date" 
#only boat sounds over date 

data_boat1 <- subset(data, treatment == "vessel")
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

data_healthy <- subset(data, treatment == "reef")
model_healthy <- glm(settled ~ date, data = data_healthy, family = binomial)
car::Anova(model_healthy, type=2)
marginal_healthy <-lsmeans(model_healthy, ~ date)
pairs(marginal_healthy, adjust="tukey")

m3 <- ggpredict(model_healthy, terms = c("date"))
plot(m3)+
  geom_point() +
  labs(title = "", x = 'reef', y= 'Larvae Settled (%)') +
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

#only no sound over date
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
data_RB <- subset(data, treatment == "reef + vessel")
model_RB <- glm(settled ~ date, data = data_RB, family = binomial)
car::Anova(model_RB, type=2)
marginal_RB <-lsmeans(model_RB, ~ date)
pairs(marginal_RB, adjust="tukey")

m6 <- ggpredict(model_RB, terms = c("date"))
plot(m6)+
  labs(x = 'Reef + Boat', 
       y= 'Larvae Settled (%)',
       title = "")


#displaying the acoustic characteristics: SPL, ADI, AEI. 

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

#looking at data by day 
data_3rd <- subset(data, date == "3-Mar")
data_4th <- subset(data, date == "4-Mar")
data_5th <- subset(data, date == "5-Mar")
data_7th <- subset(data, date == "7-Mar")

#3rd of march
model_3rd <-glm(settled ~ treatment, data = data_3rd, family = binomial)
car::Anova(model_3rd, type=2)

marginal <-lsmeans(model_3rd, ~treatment)
pairs(marginal, adjust="tukey")

m7 <- ggpredict(model_3rd, terms = c("treatment"))

plot(m7)+
  geom_point() +
  labs(title = "", x = ' ', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))

#4th of March 
model_4th <-glm(settled ~ treatment, data = data_4th, family = binomial)
car::Anova(model_4th, type=2)

#post hoc  
marginal <-lsmeans(model_4th, ~treatment)
pairs(marginal, adjust="tukey")

#creating prediction plots
m8 <- ggpredict(model_4th, terms = c("treatment"))

plot(m8)+
  geom_point() +
  labs(title = "", x = ' ', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))

#5th of March 
model_5th <-glm(settled ~ treatment, data = data_5th, family = binomial)
car::Anova(model_5th, type=2)

  
marginal <-lsmeans(model_5th, ~treatment)
pairs(marginal, adjust="tukey")


m9 <- ggpredict(model_5th, terms = c("treatment"))

plot(m9)+
  geom_point() +
  labs(title = "", x = ' ', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))

#7th of March 
model_7th <-glm(settled ~ treatment, data = data_7th, family = binomial)
car::Anova(model_7th, type=2)

marginal <-lsmeans(model_7th, ~treatment)
pairs(marginal, adjust="tukey")

m10 <- ggpredict(model_7th, terms = c("treatment"))
plot(m10)+
  geom_point() +
  labs(title = "", x = ' ', y= 'Larvae Settled (%)') +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 15))





#-------------------------------------------------------------------------------------
#new models ignore

model11 <- glmer(settled ~ treatment + date + (1|treatment:date) + (1|cup), data = data, family = binomial)

model12  <-glmer(settled ~ treatment + date + (1|treatment:date), data = data, family = binomial)
summary(model12)
car::Anova(model12, type=2)
marginal <-lsmeans(model12, ~treatment)
pairs(marginal, adjust="tukey")

model13  <-glmer(settled ~ treatment + date + (1|cup_position), data = data, family = binomial)
summary(model13)
car::Anova(model13, type=2)
marginal <-lsmeans(model13, ~treatment)
pairs(marginal, adjust="tukey")


model14  <-glmer(settled ~ treatment + date + (1|speaker), data = data, family = binomial)
summary(model14)
car::Anova(model14, type=2)
marginal <-lsmeans(model14, ~treatment)
pairs(marginal, adjust="tukey")


model15  <-glmer(settled ~ treatment + date + (1|tank), data = data, family = binomial)
summary(model15)
car::Anova(model15, type=2)
marginal <-lsmeans(model15, ~treatment)
pairs(marginal, adjust="tukey")


model16  <-glmer(settled ~ treatment + date + (1|cup_position) + (1|speaker), data = data, family = binomial)
summary(model16)
car::Anova(model16, type=2)
marginal <-lsmeans(model16, ~treatment)
pairs(marginal, adjust="tukey")


model17  <-glmer(settled ~ treatment + date + (1|speaker) + (1|tank), data = data, family = binomial)
summary(model17)
car::Anova(model17, type=2)
marginal <-lsmeans(model17, ~treatment)
pairs(marginal, adjust="tukey")


model18  <-glmer(settled ~ treatment + date + (1|cup_position) + (1|tank), data = data, family = binomial)
summary(model18)
car::Anova(model18, type=2)
marginal <-lsmeans(model18, ~treatment)
pairs(marginal, adjust="tukey")




model19  <-glmer(settled ~ treatment + date + (1|cup_position) + (1|speaker), data = data, family = binomial)
summary(model19)
car::Anova(model19, type=2)
marginal <-lsmeans(model19, ~treatment)
pairs(marginal, adjust="tukey")



model20  <-glmer(settled ~ treatment + (1|cup_position) + (1|speaker), data = data, family = binomial)
summary(model20)

model21  <-glmer(settled ~ treatment + (1|cup_position), data = data, family = binomial)
summary(model21)

model22  <-glmer(settled ~ treatment + (1|cup_position) + (1|speaker) + (1|tank), data = data, family = binomial)
summary(model22)

model23  <-glmer(settled ~ treatment + (1|date), data = data, family = binomial)
summary(model23)


model24  <-glmer(settled ~ treatment + date + (1|cup_position), data = data, family = binomial)
summary(model24)
car::Anova(model24, type=2)
marginal <-lsmeans(model24, ~treatment)
pairs(marginal, adjust="tukey")

model25  <-glmer(settled ~ treatment + date + cup_position + (1|speaker), data = data, family = binomial)
summary(model25)

model26  <-glmer(settled ~ treatment + date + cup_position + (1|tank), data = data, family = binomial)
summary(model26)

model25  <-glmer(settled ~ treatment + date + cup_position, data = data, family = binomial)
summary(model25)

model26  <-glmer(settled ~ treatment + date + (1|cup_position) + (1|speaker) + (1|tank), data = data, family = binomial)
summary(model26)
car::Anova(model26, type=2)
marginal <-lsmeans(model26, ~treatment)
pairs(marginal, adjust="tukey")


model27  <-glm(settled ~ treatment + date, data = data, family = binomial)
summary(model27)
car::Anova(model27, type=2)
marginal <-lsmeans(model27, ~treatment)
pairs(marginal, adjust="tukey")

model28  <-glmer(settled ~ treatment + date + (1|cup_position), data = data, family = binomial)
summary(model28)
car::Anova(model28, type=2)
marginal <-lsmeans(model28, ~treatment)
pairs(marginal, adjust="tukey")
