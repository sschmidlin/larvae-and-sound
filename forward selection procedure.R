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

#making dummy group as glmer needs at least one random variable 
data$dummy_group <- factor(paste(data$speaker, data$tank, sep = "_"))

# Define the initial model with the known fixed effect
base_model <- glmer(settled ~ treatment + (1|dummy_group), data = data, family = binomial)

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


