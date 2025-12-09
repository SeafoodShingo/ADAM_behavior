# Load necessary library
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


# set working directory
setwd("/Users/au708718/Library/CloudStorage/OneDrive-AarhusUniversitet/PhD/Projects/My Real Project/DATA/Analysis/DataFiles")

# Read the dataset
data <- read.csv("ADAM_newBeginning_janPVQ.csv")

# Convert the relevant columns to factors
data$Nationality <- as.factor(data$Nationality)
data$Nationality_Match <- as.factor(data$Constellation)
data$Condition_Number <- as.factor(data$error_correction)
data$Gender <- as.factor(data$Gender)
data$Gender_Match <- as.factor(data$Gender_Match)
data$order <- as.factor(data$order)

# Adaptation levels
data <- data %>%
  mutate(adaptation = case_when(
    Level %in% c(1, 25) ~ 'low',
    Level %in% c(3, 10) ~ 'high',
    TRUE ~ as.character(Level)  # This line handles any unexpected values
  ))

data$adaptation <- as.factor(data$adaptation)
# invert Joint_Gamma, so 0 becomes segregation and 1 becomes integrated. 
data$Joint_Gamma <- max(data$Joint_Gamma, na.rm = TRUE) - data$Joint_Gamma

######

# Exclude specific Participants (those who figures out the believe manipulation)
excluded_participants <- c(25, 62, 120, 127)
data_complete <- subset(data, !Participant %in% excluded_participants)

phase_data <- subset(data_complete, Condition_Number == 1)
period_data <- subset(data_complete, Condition_Number == 2)

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

### SD ITI ###
lm_model_sditi <- lm(SD_ITI ~ Nationality * Nationality_Match * Focus_Centered, data = data_complete)
# all
bc_out <- boxcox(lm_model_sditi, lambda = seq(-2, 2, by = 0.1))
optimal_lambda_sditi <- bc_out$x[which.max(bc_out$y)]
data_complete$SD_ITI_transformed <- bcPower(data_complete$SD_ITI, lambda = optimal_lambda_sditi)

### MN ITI ###
lm_model_mniti <- lm(Mn_ITI ~ Nationality * Nationality_Match * Focus_Centered, data = data_complete)
# all
bc_out <- boxcox(lm_model_mniti, lambda = seq(-2, 2, by = 0.1))
optimal_lambda_mniti <- bc_out$x[which.max(bc_out$y)]
data_complete$Mn_ITI_transformed <- bcPower(data_complete$Mn_ITI, lambda = optimal_lambda_mniti)

## #Coefficient of Variation ####
data_complete$Coeff_Variation <- with(data_complete, ifelse(Mn_ITI != 0, SD_ITI / Mn_ITI, NA))

lm_model_coef <- lm(Coeff_Variation ~ Nationality * Nationality_Match * Focus_Centered, data = data_complete)
# all
bc_out <- boxcox(lm_model_coef, lambda = seq(-2, 2, by = 0.1))
optimal_lambda_coef <- bc_out$x[which.max(bc_out$y)]
data_complete$Coeff_Variation_transformed <- bcPower(data_complete$Coeff_Variation, lambda = optimal_lambda_coef)



######------ SD SIGNED ASYNC -------########
#all
lm_model_sdAsync <- lm(SD_Signed_Async ~ Nationality * Nationality_Match * Focus_Centered, data = data_complete)
#period
lm_model_period <- lm(SD_Signed_Async ~ Nationality * Nationality_Match * Focus_Centered, data = period_data)
#phase
lm_model_phase <- lm(SD_Signed_Async ~ Nationality * Nationality_Match * Focus_Centered, data = phase_data)



# Finding the best lambda
# all
bc_out <- boxcox(lm_model_sdAsync, lambda = seq(-2, 2, by = 0.1))
optimal_lambda_sdAsync <- bc_out$x[which.max(bc_out$y)]

#period
bc_out_period <- boxcox(lm_model_period, lambda = seq(-2, 2, by = 0.1))
optimal_lambda_period <- bc_out_period$x[which.max(bc_out_period$y)]
#phase
bc_out_phase <- boxcox(lm_model_phase, lambda = seq(-2, 2, by = 0.1))
optimal_lambda_phase <- bc_out_phase$x[which.max(bc_out_phase$y)]

# Apply the Box-Cox Transformation with the optimal lambda
# all
data_complete$SD_Signed_Async_transformed <- bcPower(data_complete$SD_Signed_Async, lambda = optimal_lambda_sdAsync)
#period
period_data$SD_Signed_Async_transformed <- bcPower(period_data$SD_Signed_Async, lambda = optimal_lambda_period)
#phase
phase_data$SD_Signed_Async_transformed <- bcPower(phase_data$SD_Signed_Async, lambda = optimal_lambda_phase)


data_complete$Coeff_Variation <- with(data_complete, ifelse(Mn_ITI != 0, SD_ITI / Mn_ITI, NA))
hist(data_complete$Coeff_Variation_transformed)

# Define all variables used across all models
all_variables <- c(
  "Mn_Abs_Async_transformed",
  "SD_Signed_Async_transformed", "SD_ITI_transformed","Mn_ITI_transformed","Mn_ITI","Coeff_Variation_transformed",
  "Condition_Number", "adaptation",
  "Joint_LLE","Adapt_LLE",
  "Nationality", "Nationality_Match", "Focus_Centered", "Social_Focus_Centered", "Personal_Focus_Centered", 
  "social_old", "personal_old", "social_minus_personal_old",
  "Joint_Gamma", "Joint_Beta", "Joint_Delta", "Joint_Tn",
  "Gender_Match", "Age",
  "RYT", "PT", "IAT", "SMT",
  "Participant"  # Including the random effect variable
)

# Subset the data to include only complete cases for these variables using base R
data_subset <- data_complete[complete.cases(data_complete[, all_variables]), all_variables]



# Verify the number of observations in the subset
cat("Number of observations in the subset:", nrow(data_subset), "\n")

###########################################################




# Direct formula with up to 4-way interactions
model_zero_abs <- lmer(Mn_Abs_Async_transformed ~ 
                         (Condition_Number + adaptation + Nationality + Nationality_Match + Focus_Centered)^4 + 
                         (1 | Participant), 
                       data = data_subset)

summary(model_zero_abs)




model_zero_sd <- lmer(SD_Signed_Async_transformed ~ 
                        (Condition_Number + adaptation + Nationality + Nationality_Match + Focus_Centered)^4 + 
                        (1 | Participant), 
                      data = data_subset)

summary(model_zero_sd)

### --> Condition Level 2 is the most interesting <--- ###

# Subset the data where Condition_Number == 2
data_condition2 <- data_subset %>%
  filter(Condition_Number == 2)

data_condition1 <- data_subset %>%
  filter(Condition_Number == 1)

data_condition2 <- data_condition2 %>% filter(Joint_Beta <= 1)
data_condition2$log_Joint_Tn <- log(data_condition2$Joint_Tn)

data_condition1 <- data_condition1 %>% filter(Joint_Beta <= 1)
data_condition1$log_Joint_Tn <- log(data_condition1$Joint_Tn)

#########################################################
#########################################################
   #----- Compare LLE across Adapt and Joint -------#
#########################################################
#########################################################


library(tidyverse)
library(ggplot2)

### 1. Convert both dataframes to long format with a Condition label ###

long1 <- data_condition1 %>%
  pivot_longer(
    cols = c(Adapt_LLE, Joint_LLE),
    names_to = "Version",
    values_to = "LLE"
  ) %>%
  mutate(Condition = "Condition 1")

long2 <- data_condition2 %>%
  pivot_longer(
    cols = c(Adapt_LLE, Joint_LLE),
    names_to = "Version",
    values_to = "LLE"
  ) %>%
  mutate(Condition = "Condition 2")

# Combine both
long_all <- bind_rows(long1, long2)

### 2. Compute mean and SE for each Condition × Version ###

summary_all <- long_all %>%
  group_by(Condition, Version) %>%
  summarise(
    Mean_LLE = mean(LLE, na.rm = TRUE),
    SE_LLE = sd(LLE, na.rm = TRUE) / sqrt(sum(!is.na(LLE))),
    .groups = "drop"
  )

### 3. Plot: barplot with error bars, faceted by condition, zoomed y-axis ###

ggplot(summary_all, aes(x = Version, y = Mean_LLE, fill = Version)) +
  geom_col(width = 0.6, color = "black") +
  geom_errorbar(
    aes(ymin = Mean_LLE - SE_LLE,
        ymax = Mean_LLE + SE_LLE),
    width = 0.15
  ) +
  facet_wrap(~ Condition) +
  coord_cartesian(ylim = c(-325, -300)) +   # ZOOM HERE
  labs(
    title = "LLE Comparison: Adapt vs Joint Across Conditions",
    x = "Model version",
    y = "Mean LLE"
  ) +
  scale_fill_manual(values = c(
    "Adapt_LLE" = "#F0A07C",
    "Joint_LLE" = "#80B8D8"
  )) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 11)
  )

