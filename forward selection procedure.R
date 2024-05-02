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


# Create a unique identifier for the dummy group
data$dummy_group <- factor(paste(data$cup_position, data$speaker, data$tank, sep = "_"))

# Define the initial model with the known fixed effect
base_model <- glmer(settled ~ treatment + (1|dummy_group), data = data, family = binomial)

# List of candidate variables
candidate_variables <- c("cup_position", "speaker", "date", "tank")

# Initialize variables to store AIC values
all_models <- list()

# Loop through all possible combinations of variables
for (i in 1:length(candidate_variables)) {
  for (j in i:length(candidate_variables)) {
    fixed_vars <- candidate_variables[i:j]
    random_vars <- candidate_variables[1:j]
    
    # Loop through all possible models with the current combination of variables
    for (variable in fixed_vars) {
      # Fit the model with the added fixed effect
      fixed_formula <- as.formula(paste("settled ~ treatment +", paste(fixed_vars, collapse = "+"), "+ (1|dummy_group)"))
      fixed_model <- glmer(fixed_formula, data = data, family = binomial)
      
      # Store AIC value
      all_models[[paste("Fixed:", paste(fixed_vars, collapse = ","))]] <- AIC(fixed_model)
    }
    
    for (variable in random_vars) {
      # Fit the model with the added random effect
      random_formula <- as.formula(paste("settled ~ treatment + (1|", paste(random_vars, collapse = "+"), ") + (1|dummy_group)"))
      random_model <- glmer(random_formula, data = data, family = binomial)
      
      # Store AIC value
      all_models[[paste("Random:", paste(random_vars, collapse = ","))]] <- AIC(random_model)
    }
  }
}

# Display AIC values for all models
print(all_models)
