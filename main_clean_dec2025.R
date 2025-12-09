# Culture Shapes the Computational Mechanisms of Interpersonal Synchronization

# Main analyses script - CLEANED version

# 1. Load necessary library
library(tidyverse)
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(emmeans)
library(ggeffects)
library(car)
library(RColorBrewer)  
library(MASS)
library(interactions)

# 2. Set working directory and read the dataset
setwd("/Users/au708718/Library/CloudStorage/OneDrive-AarhusUniversitet/PhD/Projects/My Real Project/DATA/Analysis/DataFiles")
data <- read.csv("ADAM_newBeginning_janPVQ.csv")


# 3. Convert the relevant columns to factors and rescale
data$Nationality <- as.factor(data$Nationality)
data$Nationality_Match <- as.factor(data$Constellation)
data$Condition_Number <- as.factor(data$error_correction)
data$Gender <- as.factor(data$Gender)
data$Gender_Match <- as.factor(data$Gender_Match)
data$order <- as.factor(data$order)

  
  # Adaptation levels as well
  data <- data %>%
    mutate(adaptation = case_when(
      Level %in% c(1, 25) ~ 'low',
      Level %in% c(3, 10) ~ 'high',
      TRUE ~ as.character(Level)  # This line handles any unexpected values
    ))
  
  data$adaptation <- as.factor(data$adaptation)

# invert Joint_Gamma, so 0 becomes segregation and 1 becomes integrated. 
data$Joint_Gamma <- max(data$Joint_Gamma, na.rm = TRUE) - data$Joint_Gamma
  

# 4. Exclude specific Participants (those who figures out the believe manipulation)
excluded_participants <- c(25, 62, 120, 127)
data_complete <- subset(data, !Participant %in% excluded_participants)


  
phase_data <- subset(data_complete, Condition_Number == 1) # phase = fixed
period_data <- subset(data_complete, Condition_Number == 2) # period = free
  

# 5. transformation of dependent variable 

############################### 
###### Box Cox Transform ######
############################### 


  ######------ MN ABS ASYNC -------########
  
  lm_model_absAsync <- lm(Mn_Abs_Async ~ Nationality * Nationality_Match * Focus_Centered, data = data_complete)
  lm_model_period <- lm(Mn_Abs_Async ~ Nationality * Nationality_Match * Focus_Centered, data = period_data)
  lm_model_phase <- lm(Mn_Abs_Async ~ Nationality * Nationality_Match * Focus_Centered, data = phase_data)
  
  # Finding the best lambda
  # all
  bc_out <- boxcox(lm_model_absAsync, lambda = seq(-2, 2, by = 0.1))
  optimal_lambda_absAsync <- bc_out$x[which.max(bc_out$y)]
  
  # period
  bc_out_period <- boxcox(lm_model_period, lambda = seq(-2, 2, by = 0.1))
  optimal_lambda_period <- bc_out_period$x[which.max(bc_out_period$y)]
  
  # phase
  bc_out_phase <- boxcox(lm_model_phase, lambda = seq(-2, 2, by = 0.1))
  optimal_lambda_phase <- bc_out_phase$x[which.max(bc_out_phase$y)]
  
  # Apply the Box-Cox Transformation with the optimal lambda
  #all
  data_complete$Mn_Abs_Async_transformed <- bcPower(data_complete$Mn_Abs_Async, lambda = optimal_lambda_absAsync)
  
  #period
  period_data$Mn_Abs_Async_transformed <- bcPower(period_data$Mn_Abs_Async, lambda = optimal_lambda_period)
  
  #phase
  phase_data$Mn_Abs_Async_transformed <- bcPower(phase_data$Mn_Abs_Async, lambda = optimal_lambda_phase)
    

# 6. Define all variables used across all models and subset data with it
all_variables <- c(
  "Mn_Abs_Async_transformed",
  #"SD_Signed_Async_transformed", "SD_ITI_transformed","Mn_ITI_transformed","Mn_ITI","Coeff_Variation_transformed",
  "Condition_Number", "adaptation",
  "Nationality", "Nationality_Match", "Focus_Centered", "Social_Focus_Centered", "Personal_Focus_Centered", 
  "social_old", "personal_old", "social_minus_personal_old",
  "Joint_Gamma", "Joint_Beta", "Joint_Delta", "Joint_Tn",
  "Gender_Match", "Age",
  "RYT", "PT", "IAT", "SMT",
  "Participant"  # Including the random effect variable
)
data_subset <- data_complete[complete.cases(data_complete[, all_variables]), all_variables]


# 7. Subset data based on partner predictability (condition 1 or condition 2)
## Condition 2
data_condition2 <- data_subset %>%
  filter(Condition_Number == 2)

data_condition2 <- data_condition2 %>% filter(Joint_Beta <= 1)
data_condition2$log_Joint_Tn <- log(data_condition2$Joint_Tn)

## Condition 1
data_condition1 <- data_subset %>%
  filter(Condition_Number == 1)
data_condition1 <- data_condition1 %>% filter(Joint_Beta <= 1)
data_condition1$log_Joint_Tn <- log(data_condition1$Joint_Tn)



##########################################
#### Tempo-FREE, (i.e., condition 2) ####
##########################################

# first, plot difference in Async between nationalities
ggplot(data_condition2, aes(x = Nationality, 
                        y = Mn_Abs_Async_transformed,
                        fill = Nationality)) +
  geom_boxplot(width = 0.50, alpha = 0.8, outlier.alpha = 0.6) +

  
  # Your custom colors
  scale_color_manual(values = c("CHN" = "#D55E00", "DK" = "#0072B2")) +
  scale_fill_manual(values = c("CHN" = "#F0A07C", "DK" = "#80B8D8")) +
  
  labs(
    title = "Asynchrony Across Nationalities",
    x = "Nationality",
    y = "Mn_Abs_Async_transformed"
  ) +
  
  theme_minimal() +
  theme(panel.grid = element_blank())

### Nationality as cultural proxy ###


# BASELINE
model_1 <- lmer(Mn_Abs_Async_transformed ~ 1 + 
                  (1|Participant), data = data_condition2)

# Task specifications
model_2 <- lmer(Mn_Abs_Async_transformed ~ adaptation + 
                  (1|Participant), data = data_condition2)


# Culture
model_3 <- lmer(Mn_Abs_Async_transformed ~ adaptation + 
                  (Nationality * Nationality_Match * Joint_Gamma) +
                  (Nationality * Nationality_Match * Joint_Beta) +
                  (Nationality * Nationality_Match * Joint_Delta) +
                  (Nationality * Nationality_Match * log_Joint_Tn) + 
                  (1|Participant), data = data_condition2)


# Demographics
model_4 <- lmer(Mn_Abs_Async_transformed ~ adaptation + 
                  (Nationality * Nationality_Match * Joint_Gamma) +
                  (Nationality * Nationality_Match * Joint_Beta) +
                  (Nationality * Nationality_Match * Joint_Delta) +
                  (Nationality * Nationality_Match * log_Joint_Tn) + 
                  Gender_Match + Age + (1|Participant), data = data_condition2)

# Cognitive Perceptual Measures
model_5 <- lmer(Mn_Abs_Async_transformed ~ adaptation +
                  (Nationality * Nationality_Match * Joint_Gamma) +
                  (Nationality * Nationality_Match * Joint_Beta) +
                  (Nationality * Nationality_Match * Joint_Delta) +
                  (Nationality * Nationality_Match * log_Joint_Tn) + 
                  Gender_Match + Age + 
                  RYT + PT + IAT + SMT + 
                  (1|Participant), data = data_condition2)


anova(model_1, model_2, model_3, model_4, model_5)


summary(model_5)

model_period_nat <- model_5

## simplify model 5

model_51 <- lmer(Mn_Abs_Async_transformed ~ adaptation +
                   (Nationality * Nationality_Match * Joint_Gamma) +
                   (Nationality * Nationality_Match * Joint_Beta) +
                   (Nationality * Nationality_Match * Joint_Delta) +
                   (Nationality * Nationality_Match * log_Joint_Tn) + 
                   Gender_Match + Age + 
                   RYT + 
                   (1|Participant), data = data_condition2)

model_52 <- lmer(Mn_Abs_Async_transformed ~ adaptation +
                   (Nationality * Nationality_Match * Joint_Gamma) +
                   (Nationality * Nationality_Match * Joint_Beta) +
                   (Nationality * Nationality_Match * Joint_Delta) +
                   (Nationality * Nationality_Match * log_Joint_Tn) + 
                   Gender_Match + Age + 
                   RYT + PT + 
                   (1|Participant), data = data_condition2)

model_53 <- lmer(Mn_Abs_Async_transformed ~ adaptation +
                   (Nationality * Nationality_Match * Joint_Gamma) +
                   (Nationality * Nationality_Match * Joint_Beta) +
                   (Nationality * Nationality_Match * Joint_Delta) +
                   (Nationality * Nationality_Match * log_Joint_Tn) + 
                   Gender_Match + Age + 
                   RYT + PT + IAT + 
                   (1|Participant), data = data_condition2)

model_54 <- lmer(Mn_Abs_Async_transformed ~ adaptation +
                   (Nationality * Nationality_Match * Joint_Gamma) +
                   (Nationality * Nationality_Match * Joint_Beta) +
                   (Nationality * Nationality_Match * Joint_Delta) +
                   (Nationality * Nationality_Match * log_Joint_Tn) + 
                   Gender_Match + Age + 
                   RYT + PT + IAT + SMT + 
                   (1|Participant), data = data_condition2)


anova(model_51, model_52, model_53, model_54)


summary(model_5)

#### Nationality has a main effect, and interacts wiht Gamma, Delta, Tn, and as a 3-way with Beta. 


## Tempo FREE - Stats ##

#-------------------------------------------------#
######### Adaptation (period correction) ##########
#-------------------------------------------------#
data_condition2_filtered <- data_condition2
data_condition2_filtered$Nationality <- as.factor(data_condition2_filtered$Nationality)
data_condition2_filtered$Nationality_Match <- as.factor(data_condition2_filtered$Nationality_Match)

# Fit the model
model_beta <- lmer(
  Mn_Abs_Async_transformed ~ adaptation +
    Nationality * Nationality_Match * Joint_Beta +
    # Include other predictors if necessary
    Nationality * Nationality_Match * Joint_Gamma +
    Nationality * Nationality_Match * Joint_Delta +
    Nationality * Nationality_Match * log_Joint_Tn +
    Gender_Match + Age + RYT + PT +
    (1 | Participant),
  data = data_condition2_filtered,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)


model_beta <- model_5

# Estimate slopes of Joint_Beta for each combination of Nationality and Nationality_Match
slopes_beta <- emtrends(
  model_beta,
  ~ Nationality * Nationality_Match,
  var = "Joint_Beta"
)

summary(slopes_beta)

# Extract all pairwise comparisons for Nationality * Nationality_Match
pairwise_table <- emmeans(
  model_beta,
  pairwise ~ Nationality * Nationality_Match,
  var = "Joint_Beta"
)

# Display the pairwise comparisons
summary(pairwise_table$contrasts, infer = TRUE)

# Ensure Nationality_Match has meaningful labels
facet_labels <- c("Different" = "Nationality Match: Different", "Same" = "Nationality Match: Same")

# Re-plot with corrected facet labels
plot_interaction_beta <- interact_plot(
  model_beta,
  pred = "Joint_Beta",
  modx = "Nationality",
  mod2 = "Nationality_Match",
  plot.points = FALSE,  # Exclude raw data points
  interval = TRUE,      # Show confidence intervals
  int.width = 0.95,
  x.label = "Joint_Beta",
  y.label = "Mn_Abs_Async_transformed",
  legend.main = "Nationality"
) +
  theme_minimal() +
  theme(panel.grid = element_blank()) + 
  labs(
    title = "Interaction between Nationality, Nationality_Match, and Joint_Beta",
    subtitle = "Effect on Mn_Abs_Async_transformed"
  ) +
  facet_wrap(~ Nationality_Match, labeller = labeller(Nationality_Match = facet_labels)) + 
  scale_color_manual(values = c("CHN" = "#D55E00", "DK" = "#0072B2")) +
  scale_fill_manual(values = c("CHN" = "#F0A07C", "DK" = "#80B8D8"))


# Display the plot
print(plot_interaction_beta)



#--------------------------------------#
######### Temporal prediction ##########
#--------------------------------------#


# Estimate slopes of Joint_Delta for each Nationality
slopes_delta <- emtrends(
  model_5,
  ~ Nationality,
  var = "Joint_Delta"
)

# Display the estimated slopes
summary(slopes_delta)

# Compare the slopes between nationalities for Joint_Delta
contrast_delta <- contrast(slopes_delta, method = "pairwise")

# Display the results
summary(contrast_delta, infer = TRUE)

# Visualize the interaction for Joint_Delta
plot_interaction_delta <- interact_plot(
  model_52,
  pred = "Joint_Delta",
  modx = "Nationality",
  plot.points = FALSE,
  interval = TRUE,
  int.width = 0.95,
  x.label = "Joint_Delta",
  y.label = "Mn_Abs_Async_transformed",
  legend.main = "Nationality"
) +
  theme_minimal() +
  theme(panel.grid = element_blank()) + 
  labs(
    title = "Interaction between Nationality and Joint_Delta",
    subtitle = "Effect on Mn_Abs_Async_transformed"
  ) + 
  scale_color_manual(values = c("CHN" = "#D55E00", "DK" = "#0072B2")) +
  scale_fill_manual(values = c("CHN" = "#F0A07C", "DK" = "#80B8D8"))


# Display the plot
print(plot_interaction_delta)


#-----------------------------------------#
######### Self-other Integration ##########
#-----------------------------------------#


# Estimate the slopes of Joint_Gamma for each Nationality
slopes <- emtrends(
  model_5,
  ~ Nationality,
  var = "Joint_Gamma"
)

# Display the estimated slopes
summary(slopes)


# Compare the slopes between nationalities
contrast_result <- contrast(slopes, method = "pairwise")

# Display the contrast results
summary(contrast_result, infer = TRUE)


# Visualize the interaction
plot_interaction <- interact_plot(
  model_5,
  pred = "Joint_Gamma",
  modx = "Nationality",
  plot.points = FALSE,
  interval = TRUE,
  int.width = 0.95,
  x.label = "Joint_Gamma",
  y.label = "Mn_Abs_Async_transformed",
  legend.main = "Nationality"
) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +   # <-- REMOVE ALL GRIDLINES
  labs(
    title = "Interaction between Nationality and Joint_Gamma",
    subtitle = "Effect on Mn_Abs_Async_transformed"
  ) + 
  scale_color_manual(values = c("CHN" = "#D55E00", "DK" = "#0072B2")) +
  scale_fill_manual(values = c("CHN" = "#F0A07C", "DK" = "#80B8D8"))


print(plot_interaction)

#------------------------------------#
######### Time-keeper noise ##########
#------------------------------------#

# Estimate slopes of Joint_Delta for each Nationality
slopes_tn <- emtrends(
  model_5,
  ~ Nationality,
  var = "log_Joint_Tn"
)

# Display the estimated slopes
summary(slopes_tn)

# Compare the slopes between nationalities for Joint_Delta
contrast_tn <- contrast(slopes_tn, method = "pairwise")

# Display the results
summary(contrast_tn, infer = TRUE)

# Visualize the interaction for Joint_Delta
plot_interaction_tn<- interact_plot(
  model_5,
  pred = "log_Joint_Tn",
  modx = "Nationality",
  plot.points = FALSE,
  interval = TRUE,
  int.width = 0.95,
  x.label = "log_Joint_Tn",
  y.label = "Mn_Abs_Async_transformed",
  legend.main = "Nationality"
) +
  theme_minimal() +
  theme(panel.grid = element_blank()) + 
  labs(
    title = "Interaction between Nationality and log_Joint_Tn",
    subtitle = "Effect on Mn_Abs_Async_transformed"
  ) + 
  scale_color_manual(values = c("CHN" = "#D55E00", "DK" = "#0072B2")) +
  scale_fill_manual(values = c("CHN" = "#F0A07C", "DK" = "#80B8D8"))

# Display the plot
print(plot_interaction_tn)


###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################







##########################################
#### Tempo-Fixed, (i.e., condition 1) ####
##########################################


### Nationality as cultural proxy ###


# BASELINE
model_1 <- lmer(Mn_Abs_Async_transformed ~ 1 + 
                  (1|Participant), data = data_condition1)

# Task specifications
model_2 <- lmer(Mn_Abs_Async_transformed ~ adaptation + 
                  (1|Participant), data = data_condition1)


# Culture
model_3 <- lmer(Mn_Abs_Async_transformed ~ adaptation + 
                  (Nationality * Nationality_Match * Joint_Gamma) +
                  (Nationality * Nationality_Match * Joint_Beta) +
                  (Nationality * Nationality_Match * Joint_Delta) +
                  (Nationality * Nationality_Match * log_Joint_Tn) + 
                  (1|Participant), data = data_condition1)


# Demographics
model_4 <- lmer(Mn_Abs_Async_transformed ~ adaptation + 
                  (Nationality * Nationality_Match * Joint_Gamma) +
                  (Nationality * Nationality_Match * Joint_Beta) +
                  (Nationality * Nationality_Match * Joint_Delta) +
                  (Nationality * Nationality_Match * log_Joint_Tn) + 
                  Gender_Match + Age + (1|Participant), data = data_condition1)

# Cognitive Perceptual Measures
model_5 <- lmer(Mn_Abs_Async_transformed ~ adaptation +
                  (Nationality * Nationality_Match * Joint_Gamma) +
                  (Nationality * Nationality_Match * Joint_Beta) +
                  (Nationality * Nationality_Match * Joint_Delta) +
                  (Nationality * Nationality_Match * log_Joint_Tn) + 
                  Gender_Match + Age + 
                  RYT + PT + IAT + SMT + 
                  (1|Participant), data = data_condition1)


anova(model_1, model_2, model_3, model_4, model_5)


summary(model_3)

model_phase_nat <- model_3

#######
# Stats on 3-way GAMMA (self-other integration)

#########################################
data_condition1_filtered <- data_condition1
data_condition1_filtered$Nationality <- as.factor(data_condition1_filtered$Nationality)
data_condition1_filtered$Nationality_Match <- as.factor(data_condition1_filtered$Nationality_Match)

# Fit the model
model_gamma <- lmer(
  Mn_Abs_Async_transformed ~ adaptation +
    Nationality * Nationality_Match * Joint_Gamma +
    # Include other predictors if necessary
    Nationality * Nationality_Match * Joint_Beta +
    Nationality * Nationality_Match * Joint_Delta +
    Nationality * Nationality_Match * log_Joint_Tn +
    (1 | Participant),
  data = data_condition1_filtered,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Estimate slopes of Joint_Beta for each combination of Nationality and Nationality_Match
slopes_gamma <- emtrends(
  model_gamma,
  ~ Nationality * Nationality_Match,
  var = "Joint_Gamma"
)

summary(slopes_gamma)

# Extract all pairwise comparisons for Nationality * Nationality_Match
pairwise_table <- emmeans(
  model_gamma,
  pairwise ~ Nationality * Nationality_Match,
  var = "Joint_Gamma"
)

# Perform pairwise comparisons
contrast_gamma <- contrast(
  slopes_gamma,
  method = "pairwise",
  by = NULL  # Ensure all pairwise comparisons are generated
)

# Filter for the desired comparisons
desired_contrasts <- contrast_gamma[c(1, 2, 5, 6), ]  # Adjust indices to select the desired comparisons

# Apply Tukey adjustment to the selected comparisons
summary(desired_contrasts, infer = TRUE, adjust = "bonferroni")



#######
# Plotting of 3-way interaction

# Ensure Nationality_Match has meaningful labels
facet_labels <- c("Different" = "Nationality Match: Different", "Same" = "Nationality Match: Same")

# Re-plot with corrected facet labels and custom colors
plot_interaction_gamma <- interact_plot(
  model_gamma,
  pred = "Joint_Gamma",
  modx = "Nationality",
  mod2 = "Nationality_Match",
  plot.points = FALSE,
  interval = TRUE,
  int.width = 0.95,
  x.label = "Joint_Gamma",
  y.label = "Mn_Abs_Async_transformed",
  legend.main = "Nationality"
) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +     # <-- REMOVE ALL GRIDLINES
  labs(
    title = "Interaction between Nationality, Nationality_Match, and Joint_Gamma",
    subtitle = "Effect on Mn_Abs_Async_transformed"
  ) +
  facet_wrap(~ Nationality_Match, labeller = labeller(Nationality_Match = facet_labels)) +
  scale_color_manual(values = c("CHN" = "#D55E00", "DK" = "#0072B2")) +
  scale_fill_manual(values = c("CHN" = "#F0A07C", "DK" = "#80B8D8"))

print(plot_interaction_gamma)
